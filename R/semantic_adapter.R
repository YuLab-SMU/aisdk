#' @title Semantic Adapter Runtime
#' @description Internal semantic adapter protocol and registry for object-aware
#' context construction and inspection.
#' @name semantic_adapter
NULL

#' Create a Semantic Adapter
#'
#' Build a semantic adapter that can describe and inspect domain-specific R objects.
#' Adapters are registered into a semantic adapter registry and selected by
#' `supports(obj)` predicates at runtime.
#'
#' This constructor is part of the public semantic adapter authoring API for
#' extension packages.
#'
#' @param name Adapter name.
#' @param supports Function that returns `TRUE` when the adapter supports `obj`.
#' @param capabilities Character vector of supported capabilities.
#' @param priority Numeric priority; higher values win during dispatch.
#' @param describe_identity Optional function returning identity metadata.
#' @param describe_schema Optional function returning schema metadata.
#' @param describe_semantics Optional function returning semantic summary metadata.
#' @param peek Optional function for budget-aware preview generation.
#' @param slice Optional function for budget-aware slicing.
#' @param list_accessors Optional function returning recommended accessors.
#' @param estimate_cost Optional function returning compute/token/I/O estimates.
#' @param provenance Optional function returning provenance metadata.
#' @param validate_action Optional function for safety validation.
#' @param render_summary Optional function returning a human-readable summary.
#' @param render_inspection Optional function returning a human-readable inspection.
#' @details
#' Adapter callbacks are optional unless noted otherwise.
#'
#' `supports(obj)` should return a single `TRUE` or `FALSE` value and must not
#' error for unsupported objects.
#'
#' The structured callbacks `describe_identity()`, `describe_schema()`,
#' `describe_semantics()`, `estimate_cost()`, and `provenance()` should return
#' named lists when supplied.
#'
#' `list_accessors()` should return a character vector of recommended accessors.
#'
#' `validate_action()` should return a named list with fields such as `status`,
#' `reason`, `category`, and `expensive`.
#'
#' `render_summary()` and `render_inspection()` should return single character
#' strings suitable for user-facing output.
#' @return A `SemanticAdapter` object.
#' @export
create_semantic_adapter <- function(name,
                                    supports,
                                    capabilities = character(0),
                                    priority = 0,
                                    describe_identity = NULL,
                                    describe_schema = NULL,
                                    describe_semantics = NULL,
                                    peek = NULL,
                                    slice = NULL,
                                    list_accessors = NULL,
                                    estimate_cost = NULL,
                                    provenance = NULL,
                                    validate_action = NULL,
                                    render_summary = NULL,
                                    render_inspection = NULL) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    rlang::abort("Adapter name must be a non-empty string.")
  }
  if (!is.function(supports)) {
    rlang::abort("supports must be a function.")
  }

  structure(
    list(
      name = name,
      supports = supports,
      capabilities = unique(capabilities %||% character(0)),
      priority = priority %||% 0,
      describe_identity = describe_identity,
      describe_schema = describe_schema,
      describe_semantics = describe_semantics,
      peek = peek,
      slice = slice,
      list_accessors = list_accessors,
      estimate_cost = estimate_cost,
      provenance = provenance,
      validate_action = validate_action,
      render_summary = render_summary,
      render_inspection = render_inspection
    ),
    class = c("SemanticAdapter", "list")
  )
}

#' Create a Semantic Adapter Registry
#'
#' Create a registry for semantic adapters. The registry resolves the highest
#' priority adapter whose `supports(obj)` predicate matches an object.
#'
#' This constructor is part of the public semantic adapter authoring API for
#' extension packages.
#'
#' @param adapters Optional list of adapters to register on creation.
#' @details The returned registry exposes `register()`, `unregister()`,
#'   `get_adapter()`, `list_adapters()`, `register_workflow_hint()`,
#'   `list_workflow_hints()`, `resolve_workflow_hint()`, and `resolve()`
#'   methods.
#' @return A `SemanticAdapterRegistry` object.
#' @export
create_semantic_adapter_registry <- function(adapters = list()) {
  state <- new.env(parent = emptyenv())
  state$adapters <- list()
  state$workflow_hints <- list()

  ordered_adapters <- function() {
    adapters <- unname(state$adapters)
    if (!length(adapters)) {
      return(list())
    }
    priorities <- vapply(adapters, function(adapter) adapter$priority %||% 0, numeric(1))
    adapters[order(priorities, decreasing = TRUE)]
  }

  sort_adapters <- function() {
    adapters <- ordered_adapters()
    names(adapters) <- vapply(adapters, function(adapter) adapter$name, character(1))
    state$adapters <- adapters
    invisible(NULL)
  }

  registry <- list(
    register = function(adapter) {
      if (!inherits(adapter, "SemanticAdapter")) {
        rlang::abort("adapter must be a SemanticAdapter.")
      }
      state$adapters[[adapter$name]] <- adapter
      sort_adapters()
      invisible(registry)
    },
    unregister = function(name) {
      state$adapters[[name]] <- NULL
      invisible(registry)
    },
    get_adapter = function(name) {
      state$adapters[[name]]
    },
    list_adapters = function() {
      names(state$adapters)
    },
    register_workflow_hint = function(name, supports, hint_fn, priority = 0) {
      if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
        rlang::abort("Workflow hint name must be a non-empty string.")
      }
      if (!is.function(supports)) {
        rlang::abort("supports must be a function.")
      }
      if (!is.function(hint_fn)) {
        rlang::abort("hint_fn must be a function.")
      }
      state$workflow_hints[[name]] <- list(
        name = name,
        supports = supports,
        hint_fn = hint_fn,
        priority = priority %||% 0
      )
      invisible(registry)
    },
    list_workflow_hints = function() {
      names(state$workflow_hints)
    },
    resolve_workflow_hint = function(obj, goal = NULL) {
      hints <- unname(state$workflow_hints)
      if (!length(hints)) {
        return(NULL)
      }
      priorities <- vapply(hints, function(h) h$priority %||% 0, numeric(1))
      hints <- hints[order(priorities, decreasing = TRUE)]
      for (hint in hints) {
        matched <- tryCatch(isTRUE(hint$supports(obj)), error = function(e) FALSE)
        if (matched) {
          return(tryCatch(hint$hint_fn(obj, goal = goal), error = function(e) NULL))
        }
      }
      NULL
    },
    resolve = function(obj) {
      if (length(state$adapters) == 0) {
        return(NULL)
      }

      for (adapter in ordered_adapters()) {
        matched <- tryCatch(
          isTRUE(adapter$supports(obj)),
          error = function(e) FALSE
        )
        if (matched) {
          return(adapter)
        }
      }

      NULL
    }
  )

  class(registry) <- c("SemanticAdapterRegistry", "list")

  for (adapter in adapters %||% list()) {
    registry$register(adapter)
  }

  registry
}

#' Get a Semantic Adapter Registry
#'
#' Resolve or create the semantic adapter registry associated with a session or environment.
#'
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @return A `SemanticAdapterRegistry` object.
#' @export
#' @keywords internal
get_semantic_adapter_registry <- function(session = NULL, envir = NULL) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  get_or_create_semantic_adapter_registry(envir = target_envir)
}

#' Register a Semantic Adapter
#'
#' Register an adapter into a semantic adapter registry associated with a session,
#' environment, or an explicitly provided registry.
#'
#' @param adapter A `SemanticAdapter`.
#' @param registry Optional `SemanticAdapterRegistry`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @return Invisibly returns the registry.
#' @export
#' @keywords internal
register_semantic_adapter <- function(adapter,
                                      registry = NULL,
                                      session = NULL,
                                      envir = NULL) {
  target_registry <- registry %||% get_semantic_adapter_registry(session = session, envir = envir)
  target_registry$register(adapter)
  invisible(target_registry)
}

#' Register a Semantic Workflow Hint
#'
#' Register a workflow hint resolver for semantic objects.
#'
#' @param name Hint name.
#' @param supports Function that returns `TRUE` for supported objects.
#' @param hint_fn Function of `(obj, goal = NULL)` returning workflow hint metadata.
#' @param priority Numeric priority; higher values win during dispatch.
#' @param registry Optional `SemanticAdapterRegistry`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @return Invisibly returns the registry.
#' @export
#' @keywords internal
register_semantic_workflow_hint <- function(name,
                                            supports,
                                            hint_fn,
                                            priority = 0,
                                            registry = NULL,
                                            session = NULL,
                                            envir = NULL) {
  target_registry <- registry %||% get_semantic_adapter_registry(session = session, envir = envir)
  target_registry$register_workflow_hint(
    name = name,
    supports = supports,
    hint_fn = hint_fn,
    priority = priority
  )
  invisible(target_registry)
}

#' Get a Semantic Workflow Hint
#'
#' Resolve a workflow hint for an object using the active semantic adapter registry.
#'
#' @param obj The object to inspect.
#' @param goal Optional goal string used by the hint resolver.
#' @param registry Optional `SemanticAdapterRegistry`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @return A workflow hint object or `NULL`.
#' @export
#' @keywords internal
get_semantic_workflow_hint <- function(obj,
                                       goal = NULL,
                                       registry = NULL,
                                       session = NULL,
                                       envir = NULL) {
  target_registry <- registry %||% get_semantic_adapter_registry(session = session, envir = envir)
  target_registry$resolve_workflow_hint(obj, goal = goal)
}

#' Describe an Object Semantically
#'
#' Produce a structured semantic description for an object using the active registry.
#'
#' @param obj The object to describe.
#' @param name Optional display name.
#' @param registry Optional `SemanticAdapterRegistry`.
#' @param session Optional `ChatSession` or `SharedSession`.
#' @param envir Optional environment. Ignored when `session` is provided.
#' @param budget Optional budget hint passed to preview-capable adapters.
#' @return A named list containing adapter name, capabilities, and structured metadata.
#' @export
#' @keywords internal
describe_semantic_object <- function(obj,
                                     name = NULL,
                                     registry = NULL,
                                     session = NULL,
                                     envir = NULL,
                                     budget = NULL) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  target_registry <- registry %||% get_semantic_adapter_registry(session = session, envir = target_envir)
  semantic_describe_object(
    obj,
    name = name,
    registry = target_registry,
    envir = target_envir,
    budget = budget
  )
}

#' @keywords internal
default_semantic_extension_registrars <- function() {
  registrars <- getOption("aisdk.semantic_registry_registrars", list())
  if (is.null(registrars)) {
    registrars <- list()
  } else if (!is.list(registrars)) {
    registrars <- list(registrars)
  }

  Filter(is.function, registrars)
}

#' @keywords internal
apply_semantic_extension_registrars <- function(registry,
                                                include_workflow_hints = TRUE,
                                                registrars = NULL) {
  registrars <- registrars %||% list()
  for (registrar in registrars) {
    tryCatch(
      registrar(registry = registry, include_workflow_hints = include_workflow_hints),
      error = function(e) {
        warning(
          sprintf("Skipping semantic extension registrar: %s", conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  }
  registry
}

#' Create the Default Semantic Adapter Registry
#'
#' Build the package default semantic adapter registry used for semantic object
#' inspection. The default registry includes built-in generic adapters and can
#' optionally apply extension registrars.
#'
#' This function is safe for extension packages that want the standard `aisdk`
#' registry baseline before registering additional adapters.
#'
#' @param include_workflow_hints Logical; whether extension registrars should
#'   register workflow hints.
#' @param extension_registrars Optional list of registrar functions. Each
#'   registrar is called with `registry` and `include_workflow_hints`.
#' @return A `SemanticAdapterRegistry` object.
#' @export
create_default_semantic_adapter_registry <- function(include_workflow_hints = TRUE,
                                                     extension_registrars = NULL) {
  registry <- create_semantic_adapter_registry(
    adapters = list(
      create_seurat_semantic_adapter(),
      create_s4_semantic_adapter(),
      create_generic_semantic_adapter()
    )
  )

  apply_semantic_extension_registrars(
    registry = registry,
    include_workflow_hints = include_workflow_hints,
    registrars = extension_registrars %||% default_semantic_extension_registrars()
  )
}

#' Get or Create a Semantic Adapter Registry
#'
#' Resolve the semantic adapter registry from an environment, or create and
#' cache a default registry when one is not already present.
#'
#' This helper is safe for extension packages that need to share the active
#' semantic registry through a session or custom environment.
#'
#' @param envir Optional environment that stores `.semantic_adapter_registry`.
#'   When `NULL`, a new default registry is created and returned without caching.
#' @return A `SemanticAdapterRegistry` object.
#' @export
get_or_create_semantic_adapter_registry <- function(envir = NULL) {
  target_envir <- envir

  if (!is.null(target_envir) && exists(".semantic_adapter_registry", envir = target_envir, inherits = FALSE)) {
    return(get(".semantic_adapter_registry", envir = target_envir, inherits = FALSE))
  }

  registry <- create_default_semantic_adapter_registry()

  if (!is.null(target_envir) && is.environment(target_envir)) {
    assign(".semantic_adapter_registry", registry, envir = target_envir)
  }

  registry
}

#' @keywords internal
semantic_object_adapter <- function(obj, registry = NULL, envir = NULL) {
  registry <- registry %||% get_or_create_semantic_adapter_registry(envir = envir)
  adapter <- registry$resolve(obj)
  adapter %||% create_generic_semantic_adapter()
}

#' @keywords internal
semantic_describe_object <- function(obj, name = NULL, registry = NULL, envir = NULL, budget = NULL) {
  adapter <- semantic_object_adapter(obj, registry = registry, envir = envir)
  workflow_hint <- if (!is.null(registry) && !is.null(registry$resolve_workflow_hint)) {
    tryCatch(registry$resolve_workflow_hint(obj, goal = NULL), error = function(e) NULL)
  } else {
    NULL
  }

  call_adapter <- function(fn_name, default = NULL, ...) {
    fn <- adapter[[fn_name]]
    if (!is.function(fn)) {
      return(default)
    }
    tryCatch(fn(obj, ...), error = function(e) default)
  }

  list(
    adapter = adapter$name,
    capabilities = adapter$capabilities %||% character(0),
    name = name,
    identity = call_adapter("describe_identity", default = list()),
    schema = call_adapter("describe_schema", default = list()),
    semantics = call_adapter("describe_semantics", default = list()),
    preview = call_adapter("peek", default = NULL, budget = budget),
    accessors = call_adapter("list_accessors", default = character(0)),
    cost = call_adapter("estimate_cost", default = list()),
    provenance = call_adapter("provenance", default = list()),
    workflow_hint = workflow_hint
  )
}

#' @keywords internal
normalize_semantic_action_validation <- function(validation,
                                                 action,
                                                 adapter_name,
                                                 object_name = NULL,
                                                 metadata = list()) {
  validation <- validation %||% list()
  status <- validation$status %||% "allow"
  reason <- validation$reason %||% ""
  category <- validation$category %||% "read"
  expensive <- isTRUE(validation$expensive)

  utils::modifyList(
    list(
      timestamp = Sys.time(),
      type = "semantic_action_validation",
      adapter = adapter_name,
      action = action,
      object_name = object_name,
      status = status,
      reason = reason,
      category = category,
      expensive = expensive
    ),
    metadata,
    keep.null = TRUE
  )
}

#' @keywords internal
append_semantic_action_event <- function(event, session = NULL, envir = NULL) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  if (is.null(target_envir) || !is.environment(target_envir)) {
    return(invisible(event))
  }

  key <- ".semantic_action_events"
  events <- if (exists(key, envir = target_envir, inherits = FALSE)) {
    get(key, envir = target_envir, inherits = FALSE)
  } else {
    list()
  }
  assign(key, c(events, list(event)), envir = target_envir)
  invisible(event)
}

#' @keywords internal
get_semantic_action_events <- function(session = NULL, envir = NULL) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  if (is.null(target_envir) || !is.environment(target_envir)) {
    return(list())
  }
  if (!exists(".semantic_action_events", envir = target_envir, inherits = FALSE)) {
    return(list())
  }
  get(".semantic_action_events", envir = target_envir, inherits = FALSE)
}

#' @keywords internal
clear_semantic_action_events <- function(session = NULL, envir = NULL) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  if (!is.null(target_envir) && is.environment(target_envir)) {
    assign(".semantic_action_events", list(), envir = target_envir)
  }
  invisible(NULL)
}

#' @keywords internal
validate_semantic_action <- function(obj,
                                     action,
                                     registry = NULL,
                                     session = NULL,
                                     envir = NULL,
                                     collector = NULL,
                                     object_name = NULL,
                                     metadata = list()) {
  target_envir <- if (!is.null(session)) session$get_envir() else envir
  adapter <- semantic_object_adapter(obj, registry = registry, envir = target_envir)
  validation <- if (is.function(adapter$validate_action)) {
    tryCatch(adapter$validate_action(obj, action), error = function(e) {
      list(
        status = "deny",
        reason = conditionMessage(e),
        category = "validation_error",
        expensive = FALSE
      )
    })
  } else {
    list(
      status = "allow",
      reason = "No adapter validation available.",
      category = "read",
      expensive = FALSE
    )
  }

  event <- normalize_semantic_action_validation(
    validation = validation,
    action = action,
    adapter_name = adapter$name,
    object_name = object_name,
    metadata = metadata
  )
  append_semantic_action_event(event, session = session, envir = target_envir)
  if (is.function(collector)) {
    collector(event)
  }
  event
}

#' @keywords internal
semantic_render_summary <- function(obj, name = NULL, registry = NULL, envir = NULL) {
  adapter <- semantic_object_adapter(obj, registry = registry, envir = envir)
  renderer <- adapter$render_summary

  if (is.function(renderer)) {
    return(renderer(obj, name = name))
  }

  desc <- semantic_describe_object(obj, name = name, registry = registry, envir = envir)
  identity <- desc$identity %||% list()
  semantics <- desc$semantics %||% list()
  lines <- c(
    paste0("Adapter: ", desc$adapter),
    if (!is.null(identity$class)) paste0("Class: ", paste(identity$class, collapse = ", ")) else NULL,
    if (!is.null(semantics$summary)) semantics$summary else NULL
  )
  paste(lines[nzchar(lines %||% "")], collapse = "\n")
}

#' @keywords internal
semantic_render_inspection <- function(obj, name = NULL, registry = NULL, envir = NULL, head_rows = 6) {
  adapter <- semantic_object_adapter(obj, registry = registry, envir = envir)
  renderer <- adapter$render_inspection

  if (is.function(renderer)) {
    return(renderer(obj, name = name, head_rows = head_rows))
  }

  semantic_render_summary(obj, name = name, registry = registry, envir = envir)
}

#' @keywords internal
seurat_slot_or_null <- function(obj, slot_name) {
  if (!isS4(obj)) {
    return(NULL)
  }
  if (!slot_name %in% tryCatch(methods::slotNames(obj), error = function(e) character(0))) {
    return(NULL)
  }
  tryCatch(methods::slot(obj, slot_name), error = function(e) NULL)
}

#' @keywords internal
seurat_list_or_slot <- function(obj, name) {
  if (is.list(obj) && !is.null(obj[[name]])) {
    return(obj[[name]])
  }
  seurat_slot_or_null(obj, name)
}

#' @keywords internal
seurat_named_entries <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  names_vec <- tryCatch(names(x), error = function(e) character(0))
  names_vec <- names_vec %||% character(0)
  names_vec[nzchar(names_vec)]
}

#' @keywords internal
seurat_first_or_null <- function(x) {
  if (length(x %||% character(0)) == 0L) {
    return(NULL)
  }
  x[[1]]
}

#' @keywords internal
seurat_default_assay <- function(obj, assay_names = character(0)) {
  default <- call_object_accessor(
    obj,
    c("DefaultAssay"),
    default = NULL,
    package = "SeuratObject"
  )
  if (is.null(default)) {
    default <- call_object_accessor(obj, c("DefaultAssay"), default = NULL, package = "Seurat")
  }
  if (is.character(default) && length(default) > 0 && nzchar(default[[1]])) {
    return(default[[1]])
  }

  active <- seurat_slot_or_null(obj, "active.assay")
  if (is.character(active) && length(active) > 0 && nzchar(active[[1]])) {
    return(active[[1]])
  }

  seurat_first_or_null(assay_names)
}

#' @keywords internal
seurat_assay_object <- function(obj, assay = NULL) {
  assays <- seurat_list_or_slot(obj, "assays")
  if (is.null(assays)) {
    return(NULL)
  }
  assay_names <- seurat_named_entries(assays)
  assay <- assay %||% seurat_first_or_null(assay_names)
  if (is.null(assay) || !nzchar(assay) || !assay %in% assay_names) {
    return(NULL)
  }
  tryCatch(assays[[assay]], error = function(e) NULL)
}

#' @keywords internal
seurat_assay_layers <- function(assay_obj) {
  layers <- call_object_accessor(
    assay_obj,
    c("Layers"),
    default = NULL,
    package = "SeuratObject"
  )
  if (is.null(layers)) {
    layers <- call_object_accessor(assay_obj, c("Layers"), default = NULL, package = "Seurat")
  }
  if (is.character(layers) && length(layers) > 0) {
    return(layers[nzchar(layers)])
  }

  if (isS4(assay_obj)) {
    slot_names <- tryCatch(methods::slotNames(assay_obj), error = function(e) character(0))
    if ("layers" %in% slot_names) {
      return(seurat_named_entries(seurat_slot_or_null(assay_obj, "layers")))
    }
    return(intersect(slot_names, c("counts", "data", "scale.data")))
  }

  if (is.list(assay_obj)) {
    if (!is.null(assay_obj$layers)) {
      return(seurat_named_entries(assay_obj$layers))
    }
    return(intersect(names(assay_obj) %||% character(0), c("counts", "data", "scale.data")))
  }

  character(0)
}

#' @keywords internal
seurat_assay_matrix_candidates <- function(assay_obj) {
  candidates <- list()
  add_candidate <- function(value) {
    dims <- tryCatch(dim(value), error = function(e) NULL)
    if (is.numeric(dims) && length(dims) >= 2) {
      candidates[[length(candidates) + 1L]] <<- value
    }
  }

  if (isS4(assay_obj)) {
    slot_names <- tryCatch(methods::slotNames(assay_obj), error = function(e) character(0))
    for (slot_name in intersect(c("counts", "data", "scale.data"), slot_names)) {
      add_candidate(seurat_slot_or_null(assay_obj, slot_name))
    }
    layers <- seurat_slot_or_null(assay_obj, "layers")
    if (is.list(layers) || is.environment(layers)) {
      for (layer_name in seurat_named_entries(layers)) {
        add_candidate(tryCatch(layers[[layer_name]], error = function(e) NULL))
      }
    }
  } else if (is.list(assay_obj)) {
    for (entry_name in intersect(c("counts", "data", "scale.data"), names(assay_obj) %||% character(0))) {
      add_candidate(assay_obj[[entry_name]])
    }
    layers <- assay_obj$layers %||% NULL
    if (is.list(layers) || is.environment(layers)) {
      for (layer_name in seurat_named_entries(layers)) {
        add_candidate(tryCatch(layers[[layer_name]], error = function(e) NULL))
      }
    }
  }

  candidates
}

#' @keywords internal
seurat_dimensions <- function(obj, default_assay = NULL) {
  dims <- tryCatch(dim(obj), error = function(e) NULL)
  if (is.numeric(dims) && length(dims) >= 2) {
    return(list(features = as.integer(dims[[1]]), cells = as.integer(dims[[2]])))
  }

  assay_obj <- seurat_assay_object(obj, default_assay)
  assay_dims <- tryCatch(dim(assay_obj), error = function(e) NULL)
  if (is.numeric(assay_dims) && length(assay_dims) >= 2) {
    return(list(features = as.integer(assay_dims[[1]]), cells = as.integer(assay_dims[[2]])))
  }

  matrices <- seurat_assay_matrix_candidates(assay_obj)
  if (length(matrices) > 0) {
    matrix_dims <- tryCatch(dim(matrices[[1]]), error = function(e) NULL)
    if (is.numeric(matrix_dims) && length(matrix_dims) >= 2) {
      return(list(features = as.integer(matrix_dims[[1]]), cells = as.integer(matrix_dims[[2]])))
    }
  }

  meta <- seurat_list_or_slot(obj, "meta.data")
  cells <- if (is.data.frame(meta)) nrow(meta) else NA_integer_
  list(features = NA_integer_, cells = as.integer(cells))
}

#' @keywords internal
describe_seurat_like_object <- function(obj) {
  assays <- seurat_list_or_slot(obj, "assays")
  assay_names <- seurat_named_entries(assays)
  default_assay <- seurat_default_assay(obj, assay_names)
  assay_obj <- seurat_assay_object(obj, default_assay)
  reductions <- seurat_named_entries(seurat_list_or_slot(obj, "reductions"))
  images <- seurat_named_entries(seurat_list_or_slot(obj, "images"))
  meta <- seurat_list_or_slot(obj, "meta.data")
  meta_columns <- if (is.data.frame(meta)) names(meta) else character(0)
  dims <- seurat_dimensions(obj, default_assay)
  version <- seurat_slot_or_null(obj, "version")
  commands <- seurat_list_or_slot(obj, "commands")
  command_names <- seurat_named_entries(commands)

  list(
    class = class(obj),
    primary_class = class(obj)[[1]] %||% "Seurat",
    version = if (length(version) > 0) as.character(version[[1]]) else NULL,
    assays = assay_names,
    default_assay = default_assay,
    assay_class = if (!is.null(assay_obj)) class(assay_obj) else character(0),
    layers = seurat_assay_layers(assay_obj),
    assay_slots = if (isS4(assay_obj)) tryCatch(methods::slotNames(assay_obj), error = function(e) character(0)) else character(0),
    reductions = reductions,
    images = images,
    metadata_columns = meta_columns,
    cells = dims$cells,
    features = dims$features,
    commands = command_names
  )
}

#' @keywords internal
create_seurat_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "seurat",
    supports = function(obj) {
      is_semantic_class(obj, "Seurat") ||
        (isS4(obj) && all(c("assays", "meta.data") %in% tryCatch(methods::slotNames(obj), error = function(e) character(0))))
    },
    capabilities = c("identity", "schema", "semantics", "preview", "budget_estimate", "safety_checks"),
    priority = 200,
    describe_identity = function(obj) {
      info <- describe_seurat_like_object(obj)
      list(
        class = info$class,
        primary_class = info$primary_class,
        seurat_version = info$version,
        typeof = typeof(obj)
      )
    },
    describe_schema = function(obj) {
      info <- describe_seurat_like_object(obj)
      list(
        kind = "Seurat",
        assays = info$assays,
        default_assay = info$default_assay,
        assay_class = info$assay_class,
        layers = info$layers,
        assay_slots = info$assay_slots,
        reductions = info$reductions,
        images = info$images,
        metadata_columns = info$metadata_columns,
        cells = info$cells,
        features = info$features
      )
    },
    describe_semantics = function(obj) {
      info <- describe_seurat_like_object(obj)
      list(
        summary = sprintf(
          "Seurat-like single-cell object with %s assay(s), %s cell(s), and %s feature(s).",
          length(info$assays),
          if (is.na(info$cells)) "unknown" else info$cells,
          if (is.na(info$features)) "unknown" else info$features
        )
      )
    },
    estimate_cost = function(obj) {
      list(tokens = "low", compute = "low", io = "none")
    },
    provenance = function(obj) {
      list(adapter = "seurat", package = "aisdk", optional_dependencies = c("SeuratObject", "Seurat"))
    },
    validate_action = function(obj, action) {
      list(
        status = "allow",
        reason = "Seurat adapter performs read-only structural inspection.",
        category = "read",
        expensive = FALSE
      )
    },
    render_summary = function(obj, name = NULL) {
      info <- describe_seurat_like_object(obj)
      lines <- c(
        paste0("**Seurat Object** (", paste(info$class, collapse = ", "), ")"),
        "",
        paste0("Cells: ", if (is.na(info$cells)) "unknown" else info$cells),
        paste0("Features: ", if (is.na(info$features)) "unknown" else info$features),
        paste0("Assays: ", if (length(info$assays)) paste(info$assays, collapse = ", ") else "(none found)"),
        paste0("Default assay: ", info$default_assay %||% "(unknown)"),
        paste0("Assay class: ", if (length(info$assay_class)) paste(info$assay_class, collapse = ", ") else "(unknown)"),
        paste0("Layers/slots: ", if (length(info$layers)) paste(info$layers, collapse = ", ") else if (length(info$assay_slots)) paste(info$assay_slots, collapse = ", ") else "(none found)"),
        paste0("Reductions: ", if (length(info$reductions)) paste(info$reductions, collapse = ", ") else "(none found)"),
        paste0("Images: ", if (length(info$images)) paste(info$images, collapse = ", ") else "(none found)"),
        paste0("Metadata columns: ", if (length(info$metadata_columns)) paste(utils::head(info$metadata_columns, 30), collapse = ", ") else "(none found)")
      )
      if (!is.null(info$version)) {
        lines <- c(lines, paste0("Seurat object version: ", info$version))
      }
      paste(lines, collapse = "\n")
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      info <- describe_seurat_like_object(obj)
      meta_preview <- character(0)
      meta <- seurat_list_or_slot(obj, "meta.data")
      if (is.data.frame(meta) && nrow(meta) > 0) {
        meta_preview <- c(
          "",
          paste0("Metadata preview (first ", min(head_rows, nrow(meta)), " rows):"),
          utils::capture.output(print(utils::head(meta, head_rows)))
        )
      }

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(info$class, collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        paste0("Cells: ", if (is.na(info$cells)) "unknown" else info$cells),
        paste0("Features: ", if (is.na(info$features)) "unknown" else info$features),
        paste0("Assays: ", if (length(info$assays)) paste(info$assays, collapse = ", ") else "(none found)"),
        paste0("Default assay: ", info$default_assay %||% "(unknown)"),
        paste0("Assay class: ", if (length(info$assay_class)) paste(info$assay_class, collapse = ", ") else "(unknown)"),
        paste0("Layers: ", if (length(info$layers)) paste(info$layers, collapse = ", ") else "(none found)"),
        paste0("Assay slots: ", if (length(info$assay_slots)) paste(info$assay_slots, collapse = ", ") else "(none found)"),
        paste0("Reductions: ", if (length(info$reductions)) paste(info$reductions, collapse = ", ") else "(none found)"),
        paste0("Images: ", if (length(info$images)) paste(info$images, collapse = ", ") else "(none found)"),
        paste0("Metadata columns: ", if (length(info$metadata_columns)) paste(info$metadata_columns, collapse = ", ") else "(none found)"),
        if (!is.null(info$version)) paste0("Seurat object version: ", info$version) else NULL,
        if (length(info$commands)) paste0("Command history entries: ", paste(utils::head(info$commands, 20), collapse = ", ")) else NULL,
        meta_preview
      )
      paste(lines, collapse = "\n")
    }
  )
}

#' @keywords internal
create_s4_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "s4-generic",
    supports = function(obj) isS4(obj),
    capabilities = c("identity", "schema", "semantics", "preview", "budget_estimate", "safety_checks"),
    priority = -100,
    describe_identity = function(obj) {
      cls <- methods::is(obj)[1]
      list(
        class = class(obj),
        primary_class = cls,
        typeof = typeof(obj)
      )
    },
    describe_schema = function(obj) {
      slots <- tryCatch(methods::slotNames(obj), error = function(e) character(0))
      slot_classes <- setNames(
        lapply(slots, function(slot_name) class(methods::slot(obj, slot_name))),
        slots
      )
      list(
        kind = "S4",
        slots = slots,
        slot_classes = slot_classes
      )
    },
    describe_semantics = function(obj) {
      list(summary = paste("S4 object with class", paste(class(obj), collapse = ", ")))
    },
    estimate_cost = function(obj) {
      list(tokens = "low", compute = "low", io = "none")
    },
    provenance = function(obj) {
      list(
        adapter = "s4-generic",
        package = "aisdk"
      )
    },
    validate_action = function(obj, action) {
      list(
        status = "allow",
        reason = "generic S4 adapter is read-oriented",
        category = "read",
        expensive = FALSE
      )
    },
    render_summary = function(obj, name = NULL) {
      slot_names <- tryCatch(methods::slotNames(obj), error = function(e) character(0))
      slot_text <- if (length(slot_names)) {
        paste(slot_names, collapse = ", ")
      } else {
        "(no slots)"
      }
      paste0(
        "**S4 Object** (", paste(class(obj), collapse = ", "), ")\n\n",
        "**Slots:** ", slot_text
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      display_name <- name %||% "<unnamed>"
      slot_names <- tryCatch(methods::slotNames(obj), error = function(e) character(0))
      lines <- c(
        paste0("Variable: ", display_name),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        "",
        "S4 slots:"
      )

      if (!length(slot_names)) {
        lines <- c(lines, "  (no slots)")
      } else {
        for (slot_name in slot_names) {
          slot_value <- tryCatch(methods::slot(obj, slot_name), error = function(e) NULL)
          slot_class <- if (is.null(slot_value)) "NULL" else paste(class(slot_value), collapse = ", ")
          lines <- c(lines, sprintf("  - %s (%s)", slot_name, slot_class))
        }
      }

      lines <- c(lines, "", "Structure:")
      lines <- c(lines, utils::capture.output(utils::str(obj, max.level = 2, list.len = 10)))
      paste(lines, collapse = "\n")
    }
  )
}

#' Check Whether an Object Belongs to a Semantic Class
#'
#' Test class membership using S3 `inherits()` and S4 `methods::is()` fallback.
#' This helper is intended for robust adapter `supports()` predicates.
#'
#' @param obj Object to test.
#' @param class_name Class name to check.
#' @return `TRUE` if `obj` matches `class_name`; otherwise `FALSE`.
#' @export
is_semantic_class <- function(obj, class_name) {
  inherits(obj, class_name) || tryCatch(methods::is(obj, class_name), error = function(e) FALSE)
}

#' Call an Object Accessor by Candidate Function Names
#'
#' Try accessor functions in order and return the first successful result.
#' Useful for extension authors who need compatibility across optional
#' dependency APIs.
#'
#' @param obj Object passed as the first argument to the accessor.
#' @param fun_names Character vector of accessor function names to try.
#' @param default Value returned when no accessor can be called successfully.
#' @param package Optional package name to resolve accessors from first.
#' @param args Optional named list of additional arguments passed to accessor.
#' @return The accessor result or `default`.
#' @export
call_object_accessor <- function(obj, fun_names, default = NULL, package = NULL, args = list()) {
  fun_names <- fun_names %||% character(0)
  for (fun_name in fun_names) {
    fn <- NULL

    if (!is.null(package) && requireNamespace(package, quietly = TRUE)) {
      fn <- tryCatch(get(fun_name, envir = asNamespace(package)), error = function(e) NULL)
    }

    if (is.null(fn)) {
      fn <- tryCatch(get(fun_name, mode = "function"), error = function(e) NULL)
    }

    if (is.function(fn)) {
      return(tryCatch(do.call(fn, c(list(obj), args)), error = function(e) default))
    }
  }

  default
}

#' @keywords internal
safe_colnames <- function(x) {
  tryCatch(colnames(x), error = function(e) NULL)
}

#' @keywords internal
safe_rownames <- function(x) {
  tryCatch(rownames(x), error = function(e) NULL)
}

#' Render a Compact Preview String
#'
#' Convert common vector-like and tabular objects into a short text preview for
#' summaries and inspection output.
#'
#' @param x Object to preview.
#' @param max_items Maximum number of items or rows to include.
#' @return A character string preview or `NULL` when no preview is available.
#' @export
as_preview_text <- function(x, max_items = 5) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    vals <- head(x, max_items)
    return(paste(vals, collapse = ", "))
  }
  if (is.vector(x) && !is.list(x)) {
    vals <- head(as.character(x), max_items)
    return(paste(vals, collapse = ", "))
  }
  if (is.data.frame(x)) {
    return(paste(utils::capture.output(print(utils::head(x, max_items))), collapse = "\n"))
  }
  NULL
}

#' @keywords internal
create_generic_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "generic",
    supports = function(obj) TRUE,
    capabilities = c("identity", "schema", "semantics", "preview", "budget_estimate", "safety_checks"),
    priority = -1000,
    describe_identity = function(obj) {
      dims <- dim(obj)
      list(
        class = class(obj),
        typeof = typeof(obj),
        dimensions = if (is.null(dims)) NULL else as.integer(dims)
      )
    },
    describe_schema = function(obj) {
      if (is.data.frame(obj)) {
        return(list(kind = "data.frame", columns = names(obj)))
      }
      if (is.matrix(obj)) {
        return(list(kind = "matrix", nrow = nrow(obj), ncol = ncol(obj)))
      }
      if (is.list(obj) && !is.data.frame(obj)) {
        return(list(kind = "list", names = names(obj), length = length(obj)))
      }
      if (is.vector(obj) && !is.list(obj)) {
        return(list(kind = "vector", length = length(obj)))
      }
      if (is.function(obj)) {
        return(list(kind = "function", arguments = names(formals(obj))))
      }
      list(kind = "generic")
    },
    describe_semantics = function(obj) {
      list(summary = paste("Generic semantic view for", paste(class(obj), collapse = ", ")))
    },
    estimate_cost = function(obj) {
      list(
        tokens = "low",
        compute = "low",
        io = "none"
      )
    },
    provenance = function(obj) {
      list(
        adapter = "generic",
        package = "aisdk"
      )
    },
    validate_action = function(obj, action) {
      list(
        status = "allow",
        reason = "generic adapter does not impose extra restrictions",
        category = "read",
        expensive = FALSE
      )
    },
    render_summary = function(obj, name = NULL) {
      if (is.null(obj)) {
        return("Value: NULL")
      }

      if (is.data.frame(obj)) {
        return(summarize_dataframe(obj, name))
      }

      if (is.matrix(obj)) {
        return(summarize_matrix(obj, name))
      }

      if (is.list(obj) && !is.data.frame(obj)) {
        return(summarize_list(obj, name))
      }

      if (is.atomic(obj) && length(obj) > 1) {
        return(summarize_vector(obj, name))
      }

      if (is.function(obj)) {
        return(summarize_function(obj, name))
      }

      summarize_default(obj, name)
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      display_name <- name %||% "<unnamed>"
      lines <- c(paste0("Variable: ", display_name))
      lines <- c(lines, paste0("Type: ", paste(class(obj), collapse = ", ")))
      lines <- c(lines, paste0("Size: ", format(utils::object.size(obj), units = "auto")))
      lines <- c(lines, "")

      if (is.data.frame(obj)) {
        lines <- c(lines, paste0("Dimensions: ", nrow(obj), " rows x ", ncol(obj), " columns"))
        lines <- c(lines, "")
        lines <- c(lines, "Column info:")
        for (col in names(obj)) {
          col_class <- class(obj[[col]])[1]
          n_na <- sum(is.na(obj[[col]]))
          lines <- c(lines, sprintf("  - %s (%s, %d NA)", col, col_class, n_na))
        }
        lines <- c(lines, "")
        lines <- c(lines, paste0("First ", min(head_rows, nrow(obj)), " rows:"))
        lines <- c(lines, utils::capture.output(print(utils::head(obj, head_rows))))
      } else if (is.vector(obj) && !is.list(obj)) {
        lines <- c(lines, paste0("Length: ", length(obj)))
        if (is.numeric(obj)) {
          lines <- c(lines, sprintf("Range: [%.4g, %.4g]", min(obj, na.rm = TRUE), max(obj, na.rm = TRUE)))
          lines <- c(lines, sprintf("Mean: %.4g, SD: %.4g", mean(obj, na.rm = TRUE), stats::sd(obj, na.rm = TRUE)))
        }
        lines <- c(lines, paste0("NA count: ", sum(is.na(obj))))
        lines <- c(lines, "")
        lines <- c(lines, "Preview:")
        lines <- c(lines, paste(utils::head(obj, 10), collapse = ", "))
      } else {
        lines <- c(lines, "Structure:")
        lines <- c(lines, utils::capture.output(utils::str(obj, max.level = 3, list.len = 10)))
      }

      paste(lines, collapse = "\n")
    }
  )
}
