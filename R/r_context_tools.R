#' @title R Context Tools
#' @description
#' Read-only helpers and built-in tools for inspecting live R session objects,
#' functions, documentation, and source code.
#' @name r_context_tools
NULL

#' @keywords internal
is_symbolic_name <- function(name) {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    return(FALSE)
  }
  trimmed <- trimws(name)
  if (!nzchar(trimmed)) {
    return(FALSE)
  }
  !grepl("[()[:space:]]", trimmed)
}

#' @keywords internal
validate_symbolic_name <- function(name, arg_name = "name") {
  if (!is_symbolic_name(name)) {
    rlang::abort(
      sprintf("`%s` must be a single symbol-like name, not an expression.", arg_name)
    )
  }
  trimws(name)
}

#' @keywords internal
resolve_r_context_envir <- function(session = NULL, envir = NULL) {
  if (!is.null(session) && inherits(session, "ChatSession")) {
    return(session$get_envir())
  }
  envir %||% parent.frame()
}

#' @keywords internal
binding_record <- function(name,
                           object,
                           kind,
                           location,
                           package = NULL,
                           namespace = NULL,
                           exported = NA,
                           provenance = list()) {
  list(
    name = name,
    kind = kind,
    package = package,
    location = location,
    exported = exported,
    namespace = namespace,
    object = object,
    provenance = provenance
  )
}

#' @keywords internal
classify_binding_kind <- function(object) {
  if (is.function(object)) {
    "function"
  } else {
    "object"
  }
}

#' @keywords internal
resolve_search_path_binding <- function(name) {
  entries <- search()
  for (entry in entries) {
    if (identical(entry, ".GlobalEnv")) {
      next
    }
    env <- tryCatch(as.environment(entry), error = function(e) NULL)
    if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
      next
    }

    object <- get(name, envir = env, inherits = FALSE)
    package_name <- if (startsWith(entry, "package:")) sub("^package:", "", entry) else NULL
    namespace_name <- package_name
    exported <- if (!is.null(package_name) && requireNamespace(package_name, quietly = TRUE)) {
      name %in% getNamespaceExports(package_name)
    } else {
      NA
    }

    return(binding_record(
      name = name,
      object = object,
      kind = classify_binding_kind(object),
      location = "search_path",
      package = package_name,
      namespace = namespace_name,
      exported = exported,
      provenance = list(search_path_entry = entry)
    ))
  }

  NULL
}

#' @keywords internal
resolve_namespace_binding <- function(name, namespace_name) {
  if (!requireNamespace(namespace_name, quietly = TRUE)) {
    return(NULL)
  }
  ns <- tryCatch(asNamespace(namespace_name), error = function(e) NULL)
  if (is.null(ns) || !exists(name, envir = ns, inherits = FALSE)) {
    return(NULL)
  }

  object <- get(name, envir = ns, inherits = FALSE)
  binding_record(
    name = name,
    object = object,
    kind = classify_binding_kind(object),
    location = "namespace",
    package = namespace_name,
    namespace = namespace_name,
    exported = name %in% getNamespaceExports(namespace_name),
    provenance = list(source = "namespace")
  )
}

#' Resolve an R Binding
#'
#' Resolve a symbol-like name against the live session environment, attached
#' search path, and loaded namespaces.
#'
#' @param name Symbol-like name to resolve.
#' @param package Optional package/namespace name to search first.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param prefer Preferred kind: `"auto"`, `"object"`, or `"function"`.
#' @return A binding record list or `NULL` if not found.
#' @export
resolve_r_binding <- function(name,
                              package = NULL,
                              session = NULL,
                              envir = NULL,
                              prefer = c("auto", "object", "function")) {
  name <- validate_symbolic_name(name)
  prefer <- match.arg(prefer)
  target_envir <- resolve_r_context_envir(session = session, envir = envir)

  choose_candidate <- function(candidate, fallback = NULL) {
    if (is.null(candidate)) {
      return(fallback)
    }
    if (identical(prefer, "auto") || identical(candidate$kind, prefer)) {
      return(candidate)
    }
    fallback %||% candidate
  }

  fallback <- NULL

  if (!is.null(package)) {
    return(resolve_namespace_binding(name, package))
  }

  if (!is.null(target_envir) && is.environment(target_envir) &&
      exists(name, envir = target_envir, inherits = FALSE)) {
    object <- get(name, envir = target_envir, inherits = FALSE)
    candidate <- binding_record(
      name = name,
      object = object,
      kind = classify_binding_kind(object),
      location = "session_env",
      package = NULL,
      namespace = NULL,
      exported = NA,
      provenance = list(source = "session_env")
    )
    chosen <- choose_candidate(candidate, fallback = fallback)
    if (!identical(chosen, fallback)) {
      return(chosen)
    }
    fallback <- chosen
  }

  search_candidate <- resolve_search_path_binding(name)
  chosen <- choose_candidate(search_candidate, fallback = fallback)
  if (!identical(chosen, fallback) && !is.null(chosen)) {
    return(chosen)
  }
  fallback <- chosen

  for (namespace_name in loadedNamespaces()) {
    candidate <- resolve_namespace_binding(name, namespace_name)
    chosen <- choose_candidate(candidate, fallback = fallback)
    if (!identical(chosen, fallback) && !is.null(chosen)) {
      return(chosen)
    }
    fallback <- chosen
  }

  fallback
}

#' @keywords internal
format_object_size <- function(object) {
  format(utils::object.size(object), units = "auto")
}

#' List Live R Objects
#'
#' List live objects visible in the session environment.
#'
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param pattern Optional regex pattern used to filter names.
#' @param include_hidden Logical; whether to include names starting with `"."`.
#' @param limit Maximum number of rows to return.
#' @return A data frame with object metadata.
#' @export
list_r_objects <- function(session = NULL,
                           envir = NULL,
                           pattern = NULL,
                           include_hidden = FALSE,
                           limit = 50L) {
  target_envir <- resolve_r_context_envir(session = session, envir = envir)
  if (is.null(target_envir) || !is.environment(target_envir)) {
    return(data.frame(
      name = character(0),
      class = character(0),
      type = character(0),
      size = character(0),
      stringsAsFactors = FALSE
    ))
  }

  names_vec <- ls(target_envir, all.names = include_hidden, pattern = pattern %||% "")
  if (!isTRUE(include_hidden)) {
    names_vec <- names_vec[!grepl("^\\.", names_vec)]
  }
  if (length(names_vec) > limit) {
    names_vec <- names_vec[seq_len(limit)]
  }
  if (length(names_vec) == 0) {
    return(data.frame(
      name = character(0),
      class = character(0),
      type = character(0),
      size = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names_vec, function(object_name) {
    object <- get(object_name, envir = target_envir, inherits = FALSE)
    data.frame(
      name = object_name,
      class = paste(class(object), collapse = ", "),
      type = typeof(object),
      size = format_object_size(object),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' @keywords internal
format_object_table <- function(df) {
  if (nrow(df) == 0) {
    return("No live R objects found.")
  }

  lines <- c(
    sprintf("Found %d live R object(s):", nrow(df)),
    "",
    apply(df, 1, function(row) {
      sprintf("- %s | class=%s | type=%s | size=%s", row[["name"]], row[["class"]], row[["type"]], row[["size"]])
    })
  )
  paste(lines, collapse = "\n")
}

#' Inspect a Live R Object
#'
#' Inspect a symbol resolved from the live session environment.
#'
#' @param name Symbol-like object name.
#' @param detail One of `"summary"`, `"full"`, or `"structured"`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param head_rows Maximum preview rows for inspection renderers.
#' @return A character string for `"summary"`/`"full"` or a structured list for
#'   `"structured"`.
#' @export
inspect_r_object <- function(name,
                             detail = c("summary", "full", "structured"),
                             session = NULL,
                             envir = NULL,
                             head_rows = 6L) {
  detail <- match.arg(detail)
  binding <- resolve_r_binding(name, session = session, envir = envir, prefer = "object")
  if (is.null(binding)) {
    rlang::abort(sprintf("Object `%s` was not found.", name))
  }
  if (!identical(binding$kind, "object")) {
    rlang::abort(sprintf("`%s` resolves to a function, not an object.", name))
  }

  validation <- validate_semantic_action(
    binding$object,
    action = "inspect_object",
    session = session,
    envir = envir,
    object_name = name
  )
  if (identical(validation$status, "deny")) {
    rlang::abort(sprintf("Inspection denied for `%s`: %s", name, validation$reason %||% "Unknown reason"))
  }

  prefix <- if (identical(validation$status, "warn") && nzchar(validation$reason %||% "")) {
    paste0("Warning: ", validation$reason, "\n\n")
  } else {
    ""
  }

  result <- switch(detail,
    summary = semantic_render_summary(binding$object, name = name, envir = resolve_r_context_envir(session = session, envir = envir)),
    full = semantic_render_inspection(binding$object, name = name, envir = resolve_r_context_envir(session = session, envir = envir), head_rows = head_rows),
    structured = describe_semantic_object(binding$object, name = name, session = session, envir = envir)
  )

  if (is.character(result)) {
    paste0(prefix, result)
  } else {
    result
  }
}

#' @keywords internal
format_signature <- function(fn, name = NULL) {
  fn_name <- name %||% "<anonymous>"
  fmls <- tryCatch(formals(fn), error = function(e) NULL)
  if (is.null(fmls)) {
    return(sprintf("%s(<primitive>)", fn_name))
  }
  if (length(fmls) == 0) {
    return(sprintf("%s()", fn_name))
  }

  arg_text <- vapply(names(fmls), function(arg_name) {
    if (is.symbol(fmls[[arg_name]]) && identical(as.character(fmls[[arg_name]]), "")) {
      arg_name
    } else {
      paste0(arg_name, " = ", paste(deparse(fmls[[arg_name]], width.cutoff = 500L), collapse = ""))
    }
  }, character(1))

  sprintf("%s(%s)", fn_name, paste(arg_text, collapse = ", "))
}

#' @keywords internal
is_s3_generic_function <- function(fn) {
  if (!is.function(fn) || is.primitive(fn)) {
    return(FALSE)
  }
  body_text <- tryCatch(paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n"), error = function(e) "")
  grepl("UseMethod(", body_text, fixed = TRUE)
}

#' @keywords internal
function_kind <- function(fn, name = NULL, package = NULL) {
  base_kind <- if (is.primitive(fn)) {
    if (identical(typeof(fn), "special")) "special" else "primitive"
  } else {
    "closure"
  }

  s4_generic <- FALSE
  if (!is.null(name) && nzchar(name)) {
    s4_where <- if (!is.null(package) && requireNamespace(package, quietly = TRUE)) {
      asNamespace(package)
    } else {
      NULL
    }
    s4_generic <- tryCatch(methods::isGeneric(name, where = s4_where), error = function(e) FALSE)
  }
  s3_generic <- is_s3_generic_function(fn)

  details <- c(base_kind)
  if (isTRUE(s3_generic)) {
    details <- c(details, "S3 generic")
  }
  if (isTRUE(s4_generic)) {
    details <- c(details, "S4 generic")
  }

  paste(details, collapse = ", ")
}

#' Inspect an R Function
#'
#' Inspect function signature and provenance without dumping full source by
#' default.
#'
#' @param name Function name.
#' @param package Optional package/namespace name.
#' @param detail One of `"summary"` or `"full"`.
#' @param include_methods Logical; whether to include S3/S4 method previews.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @return A character string inspection.
#' @export
inspect_r_function <- function(name,
                               package = NULL,
                               detail = c("summary", "full"),
                               include_methods = FALSE,
                               session = NULL,
                               envir = NULL) {
  detail <- match.arg(detail)
  binding <- resolve_r_binding(name, package = package, session = session, envir = envir, prefer = "function")
  if (is.null(binding)) {
    rlang::abort(sprintf("Function `%s` was not found.", name))
  }
  if (!identical(binding$kind, "function")) {
    rlang::abort(sprintf("`%s` resolves to an object, not a function.", name))
  }

  fn <- binding$object
  signature <- format_signature(fn, name = name)
  kind <- function_kind(fn, name = name, package = binding$package %||% package)
  env_label <- tryCatch(environmentName(environment(fn)), error = function(e) "")
  if (!nzchar(env_label)) {
    env_label <- paste(class(environment(fn)), collapse = ", ")
  }

  lines <- c(
    sprintf("Function: %s", name),
    sprintf("Signature: %s", signature),
    sprintf("Kind: %s", kind),
    sprintf("Location: %s", binding$location %||% "unknown"),
    if (!is.null(binding$package)) sprintf("Package: %s", binding$package) else NULL,
    if (!is.null(binding$namespace)) sprintf("Namespace: %s", binding$namespace) else NULL,
    sprintf("Environment: %s", env_label)
  )

  if (identical(detail, "full")) {
    param_docs <- get_param_docs(name, package = binding$package %||% package)
    if (length(param_docs) > 0) {
      lines <- c(lines, "", "Documented parameters:")
      lines <- c(lines, vapply(names(param_docs), function(param_name) {
        sprintf("- %s: %s", param_name, trim_context_preview(param_docs[[param_name]], max_chars = 140L))
      }, character(1)))
    }
  }

  if (isTRUE(include_methods) && nzchar(name)) {
    method_lines <- tryCatch(utils::methods(name), error = function(e) character(0))
    if (length(method_lines) > 0) {
      preview <- utils::head(method_lines, 10L)
      lines <- c(lines, "", "Method preview:", paste0("- ", preview))
      if (length(method_lines) > length(preview)) {
        lines <- c(lines, sprintf("- ... %d more method(s)", length(method_lines) - length(preview)))
      }
    }
  }

  paste(lines, collapse = "\n")
}

#' @keywords internal
get_rd_tags <- function(rd) {
  vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
}

#' @keywords internal
find_rd_section <- function(rd, tag) {
  tags <- get_rd_tags(rd)
  match <- which(tags == tag)
  if (length(match) == 0) {
    return(NULL)
  }
  rd[[match[1]]]
}

#' @keywords internal
render_rd_text <- function(rd_obj) {
  if (is.null(rd_obj)) {
    return("")
  }
  paste(capture.output(tools::Rd2txt(rd_obj, fragment = TRUE)), collapse = "\n")
}

#' @keywords internal
flatten_rd_fragment <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (is.character(x)) {
    return(paste(x, collapse = ""))
  }
  if (!is.list(x)) {
    return(as.character(x))
  }

  tag <- attr(x, "Rd_tag") %||% NULL
  if (identical(tag, "\\S3method") || identical(tag, "\\method")) {
    pieces <- vapply(x, flatten_rd_fragment, character(1))
    return(paste(pieces[nzchar(pieces)], collapse = "."))
  }
  if (identical(tag, "\\dots")) {
    return("...")
  }
  if (identical(tag, "\\item")) {
    key <- flatten_rd_fragment(x[[1]])
    value <- flatten_rd_fragment(x[[2]])
    return(paste0(key, ": ", value))
  }

  paste(vapply(x, flatten_rd_fragment, character(1)), collapse = "")
}

#' @keywords internal
render_rd_plain <- function(rd_obj, preserve_newlines = FALSE) {
  text <- flatten_rd_fragment(rd_obj)
  text <- gsub("[ \t]+", " ", text)
  if (!isTRUE(preserve_newlines)) {
    text <- gsub("\\s+", " ", text)
  } else {
    text <- gsub("[ \t]*\n[ \t]*", "\n", text)
    text <- gsub("\n{3,}", "\n\n", text)
  }
  trimws(text)
}

#' @keywords internal
render_rd_arguments <- function(args_section) {
  if (is.null(args_section) || !is.list(args_section)) {
    return("")
  }
  items <- Filter(function(x) identical(attr(x, "Rd_tag") %||% "", "\\item"), args_section)
  if (length(items) == 0) {
    return("")
  }
  lines <- vapply(items, function(item) {
    key <- trimws(flatten_rd_fragment(item[[1]]))
    value <- trimws(flatten_rd_fragment(item[[2]]))
    sprintf("- %s: %s", key, value)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' @keywords internal
find_rd_topic <- function(topic, package = NULL, session = NULL, envir = NULL) {
  topic <- validate_symbolic_name(topic, arg_name = "topic")

  candidate_packages <- character(0)
  if (!is.null(package) && nzchar(package)) {
    candidate_packages <- c(candidate_packages, package)
  } else {
    binding <- tryCatch(resolve_r_binding(topic, session = session, envir = envir, prefer = "function"), error = function(e) NULL)
    if (!is.null(binding$package)) {
      candidate_packages <- c(candidate_packages, binding$package)
    }
    candidate_packages <- c(candidate_packages, .packages(), loadedNamespaces())
  }
  candidate_packages <- unique(candidate_packages[nzchar(candidate_packages)])

  for (pkg in candidate_packages) {
    db <- tryCatch(tools::Rd_db(pkg), error = function(e) NULL)
    if (is.null(db)) {
      next
    }

    direct <- db[[paste0(topic, ".Rd")]]
    if (!is.null(direct)) {
      return(list(package = pkg, topic = topic, rd = direct))
    }

    for (rd_name in names(db)) {
      rd <- db[[rd_name]]
      tags <- get_rd_tags(rd)
      aliases <- rd[tags == "\\alias"]
      if (length(aliases) == 0) {
        next
      }
      matched <- vapply(aliases, function(alias_obj) {
        trimws(render_rd_text(alias_obj))
      }, character(1))
      if (topic %in% matched) {
        return(list(package = pkg, topic = topic, rd = rd))
      }
    }
  }

  NULL
}

#' Get R Documentation
#'
#' Resolve an Rd topic and return a selected section.
#'
#' @param name Topic or function name.
#' @param package Optional package/namespace name.
#' @param section One of `"summary"`, `"usage"`, `"arguments"`, `"details"`,
#'   `"value"`, `"examples"`, or `"full"`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param max_chars Maximum characters to return.
#' @return A character string with the requested documentation.
#' @export
get_r_documentation <- function(name,
                                package = NULL,
                                section = c("summary", "usage", "arguments", "details", "value", "examples", "full"),
                                session = NULL,
                                envir = NULL,
                                max_chars = 4000L) {
  section <- match.arg(section)
  name <- validate_symbolic_name(name)
  topic <- find_rd_topic(name, package = package, session = session, envir = envir)

  if (is.null(topic)) {
    binding <- tryCatch(resolve_r_binding(name, package = package, session = session, envir = envir, prefer = "function"), error = function(e) NULL)
    fallback <- c(
      sprintf("No installed Rd documentation found for `%s`.", name),
      if (!is.null(binding) && identical(binding$kind, "function")) {
        c(
          "",
          "Fallback function summary:",
          inspect_r_function(name, package = binding$package %||% package, detail = "summary", include_methods = FALSE, session = session, envir = envir)
        )
      } else {
        NULL
      }
    )
    return(paste(fallback, collapse = "\n"))
  }

  rd <- topic$rd
  title <- trim_context_preview(render_rd_text(find_rd_section(rd, "\\title")), max_chars = 200L)
  description <- trim_context_preview(render_rd_text(find_rd_section(rd, "\\description")), max_chars = 500L)
  usage <- render_rd_plain(find_rd_section(rd, "\\usage"), preserve_newlines = TRUE)
  arguments <- render_rd_arguments(find_rd_section(rd, "\\arguments"))
  details <- render_rd_plain(find_rd_section(rd, "\\details"), preserve_newlines = TRUE)
  value <- render_rd_plain(find_rd_section(rd, "\\value"), preserve_newlines = TRUE)
  examples <- render_rd_plain(find_rd_section(rd, "\\examples"), preserve_newlines = TRUE)
  full_text <- paste(c(
    if (nzchar(title)) paste("Title:", title) else NULL,
    if (nzchar(description)) paste("Description:", description) else NULL,
    if (nzchar(usage)) c("Usage:", usage) else NULL,
    if (nzchar(arguments)) c("Arguments:", arguments) else NULL,
    if (nzchar(details)) c("Details:", details) else NULL,
    if (nzchar(value)) c("Value:", value) else NULL,
    if (nzchar(examples)) c("Examples:", examples) else NULL
  ), collapse = "\n\n")

  out <- switch(section,
    summary = paste(c(
      sprintf("Topic: %s::%s", topic$package, topic$topic),
      if (nzchar(title)) paste("Title:", title) else NULL,
      if (nzchar(description)) paste("Description:", description) else NULL,
      if (nzchar(usage)) c("", "Usage:", usage) else NULL
    ), collapse = "\n"),
    usage = usage,
    arguments = arguments,
    details = details,
    value = value,
    examples = examples,
    full = full_text
  )

  if (nchar(out, type = "chars") > max_chars) {
    out <- paste0(substr(out, 1L, max_chars - 3L), "...")
  }
  out
}

#' @keywords internal
resolve_s3_method <- function(generic, class_name, package = NULL) {
  if (!nzchar(generic) || !nzchar(class_name)) {
    return(NULL)
  }
  envir <- if (!is.null(package) && requireNamespace(package, quietly = TRUE)) asNamespace(package) else parent.frame()
  tryCatch(utils::getS3method(generic, class_name, optional = TRUE, envir = envir), error = function(e) NULL)
}

#' Get R Source
#'
#' Resolve function source text for a closure or method when available.
#'
#' @param name Function or generic name.
#' @param package Optional package/namespace name.
#' @param method Optional S3 method class name.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param max_lines Maximum lines to return.
#' @return A character string source preview or structured fallback message.
#' @export
get_r_source <- function(name,
                         package = NULL,
                         method = NULL,
                         session = NULL,
                         envir = NULL,
                         max_lines = 120L) {
  name <- validate_symbolic_name(name)

  fn <- NULL
  label <- name
  binding <- NULL

  if (!is.null(method) && nzchar(method)) {
    fn <- resolve_s3_method(name, method, package = package)
    label <- sprintf("%s.%s", name, method)
  }

  if (is.null(fn)) {
    binding <- resolve_r_binding(name, package = package, session = session, envir = envir, prefer = "function")
    if (is.null(binding)) {
      rlang::abort(sprintf("Function `%s` was not found.", name))
    }
    fn <- binding$object
  }

  if (is.null(fn)) {
    return(sprintf("No source implementation is available for `%s`.", label))
  }
  if (is.primitive(fn)) {
    kind <- if (identical(typeof(fn), "special")) "special primitive" else "primitive"
    return(sprintf("Source for `%s` is not available in R because it is a %s.", label, kind))
  }

  source_lines <- tryCatch(deparse(fn, width.cutoff = 500L), error = function(e) character(0))
  if (length(source_lines) == 0) {
    return(sprintf("Unable to deparse source for `%s`.", label))
  }
  if (length(source_lines) > max_lines) {
    source_lines <- c(source_lines[seq_len(max_lines)], sprintf("... [%d more line(s) truncated]", length(source_lines) - max_lines))
  }

  header <- c(
    sprintf("Source: %s", label),
    if (!is.null(binding$package)) sprintf("Package: %s", binding$package) else NULL,
    if (!is.null(binding$location)) sprintf("Location: %s", binding$location) else NULL,
    ""
  )

  paste(c(header, source_lines), collapse = "\n")
}

#' Create R Context Tools
#'
#' Create built-in read-only tools for live R context inspection.
#'
#' @return A list of Tool objects.
#' @export
create_r_context_tools <- function() {
  list(
    tool(
      name = "list_r_objects",
      description = paste(
        "List live R objects currently stored in the session environment.",
        "Use this before inspecting a specific object when you need to see what is available."
      ),
      parameters = z_object(
        pattern = z_string("Optional regex pattern used to filter object names", nullable = TRUE),
        include_hidden = z_boolean("Whether to include hidden names starting with '.'", nullable = TRUE),
        limit = z_integer("Maximum number of objects to return", nullable = TRUE)
      ),
      execute = function(args) {
        objects <- list_r_objects(
          envir = args$.envir,
          pattern = args$pattern %||% NULL,
          include_hidden = args$include_hidden %||% FALSE,
          limit = args$limit %||% 50L
        )
        format_object_table(objects)
      }
    ),
    tool(
      name = "inspect_r_object",
      description = paste(
        "Inspect a live R object from the session environment.",
        "Use this for data frames, model objects, lists, Bioconductor containers, and other live session objects."
      ),
      parameters = z_object(
        name = z_string("Object name to inspect"),
        detail = z_enum(c("summary", "full", "structured"), description = "Inspection detail level", default = "summary"),
        head_rows = z_integer("Maximum preview rows for tabular inspections", nullable = TRUE)
      ),
      execute = function(args) {
        inspect_r_object(
          name = args$name,
          detail = args$detail %||% "summary",
          envir = args$.envir,
          head_rows = args$head_rows %||% 6L
        )
      }
    ),
    tool(
      name = "inspect_r_function",
      description = paste(
        "Inspect an R function's signature, provenance, and optional method preview.",
        "Use this before reading source when you first need to understand what a function is."
      ),
      parameters = z_object(
        name = z_string("Function name to inspect"),
        package = z_string("Optional package/namespace name", nullable = TRUE),
        detail = z_enum(c("summary", "full"), description = "Inspection detail level", default = "summary"),
        include_methods = z_boolean("Whether to include a method preview for S3/S4 generics", nullable = TRUE)
      ),
      execute = function(args) {
        inspect_r_function(
          name = args$name,
          package = args$package %||% NULL,
          detail = args$detail %||% "summary",
          include_methods = args$include_methods %||% FALSE,
          envir = args$.envir
        )
      }
    ),
    tool(
      name = "get_r_documentation",
      description = paste(
        "Read installed R documentation for a function or topic.",
        "Use this when you need usage, arguments, details, value, examples, or a concise help-page summary."
      ),
      parameters = z_object(
        name = z_string("Function or topic name"),
        package = z_string("Optional package/namespace name", nullable = TRUE),
        section = z_enum(c("summary", "usage", "arguments", "details", "value", "examples", "full"), description = "Help section to return", default = "summary"),
        max_chars = z_integer("Maximum number of characters to return", nullable = TRUE)
      ),
      execute = function(args) {
        get_r_documentation(
          name = args$name,
          package = args$package %||% NULL,
          section = args$section %||% "summary",
          envir = args$.envir,
          max_chars = args$max_chars %||% 4000L
        )
      }
    ),
    tool(
      name = "get_r_source",
      description = paste(
        "Read source code for an R function or S3 method when available.",
        "Use this when you need implementation details instead of only signature or help-page information."
      ),
      parameters = z_object(
        name = z_string("Function or generic name"),
        package = z_string("Optional package/namespace name", nullable = TRUE),
        method = z_string("Optional S3 method class name", nullable = TRUE),
        max_lines = z_integer("Maximum number of source lines to return", nullable = TRUE)
      ),
      execute = function(args) {
        get_r_source(
          name = args$name,
          package = args$package %||% NULL,
          method = args$method %||% NULL,
          envir = args$.envir,
          max_lines = args$max_lines %||% 120L
        )
      }
    )
  )
}
