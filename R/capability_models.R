#' @title Capability Model Routes
#' @description
#' Configure model routes for task capabilities such as vision inspection,
#' image generation, or code review. These routes let a session keep one
#' default chat model while specific capabilities use better-suited models.
#' @name capability_models
NULL

.capability_model_env <- new.env(parent = emptyenv())
.capability_model_env$routes <- NULL

#' @keywords internal
normalize_capability_name <- function(capability) {
  if (!is.character(capability) || length(capability) != 1) {
    rlang::abort("`capability` must be a single character string.")
  }
  capability <- trimws(capability)
  if (!nzchar(capability)) {
    rlang::abort("`capability` must be a non-empty string.")
  }
  tolower(capability)
}

#' @keywords internal
is_model_ref <- function(model) {
  (is.character(model) && length(model) == 1 && nzchar(trimws(model))) ||
    inherits(model, "LanguageModelV1") ||
    inherits(model, "EmbeddingModelV1") ||
    inherits(model, "ImageModelV1")
}

#' @keywords internal
normalize_model_ref <- function(model, arg = "`model`") {
  if (is.character(model) && length(model) == 1) {
    model <- trimws(model)
  }
  if (!is_model_ref(model)) {
    rlang::abort(paste0(arg, " must be a non-empty model ID string or model object."))
  }
  model
}

#' @keywords internal
normalize_capability_model_type <- function(type = "auto") {
  type <- type %||% "auto"
  if (!is.character(type) || length(type) != 1) {
    rlang::abort("`type` must be one of 'auto', 'language', 'embedding', or 'image'.")
  }
  type <- tolower(trimws(type))
  if (!type %in% c("auto", "language", "embedding", "image")) {
    rlang::abort("`type` must be one of 'auto', 'language', 'embedding', or 'image'.")
  }
  type
}

#' @keywords internal
infer_model_ref_type <- function(model) {
  if (inherits(model, "LanguageModelV1")) {
    return("language")
  }
  if (inherits(model, "EmbeddingModelV1")) {
    return("embedding")
  }
  if (inherits(model, "ImageModelV1")) {
    return("image")
  }
  NULL
}

#' @keywords internal
normalize_required_model_capabilities <- function(required_model_capabilities = NULL) {
  required_model_capabilities <- required_model_capabilities %||% character(0)
  required_model_capabilities <- unique(as.character(required_model_capabilities))
  required_model_capabilities <- trimws(required_model_capabilities)
  required_model_capabilities[nzchar(required_model_capabilities)]
}

#' @keywords internal
create_capability_model_route <- function(model,
                                          type = "auto",
                                          required_model_capabilities = NULL) {
  model <- normalize_model_ref(model)
  type <- normalize_capability_model_type(type)
  inferred <- infer_model_ref_type(model)
  if (identical(type, "auto") && !is.null(inferred)) {
    type <- inferred
  }

  list(
    model = model,
    type = type,
    required_model_capabilities = normalize_required_model_capabilities(required_model_capabilities)
  )
}

#' @keywords internal
normalize_capability_model_route <- function(route) {
  if (is_model_ref(route)) {
    return(create_capability_model_route(route))
  }

  if (!is.list(route)) {
    rlang::abort("Capability model routes must be model IDs, model objects, or route lists.")
  }

  model <- route$model %||% route$id %||% NULL
  if (is.null(model)) {
    rlang::abort("Capability model route lists must include `model`.")
  }

  create_capability_model_route(
    model = model,
    type = route$type %||% "auto",
    required_model_capabilities =
      route$required_model_capabilities %||%
        route$requires_model_capabilities %||%
        route$required_capabilities %||%
        NULL
  )
}

#' @keywords internal
normalize_capability_model_routes <- function(routes = NULL) {
  routes <- routes %||% list()
  if (length(routes) == 0) {
    return(list())
  }
  if (!is.list(routes) || is.null(names(routes)) || any(!nzchar(names(routes)))) {
    rlang::abort("Capability model routes must be a named list.")
  }

  normalized <- list()
  for (name in names(routes)) {
    normalized[[normalize_capability_name(name)]] <- normalize_capability_model_route(routes[[name]])
  }
  normalized
}

#' @keywords internal
get_capability_model_routes <- function() {
  normalize_capability_model_routes(
    .capability_model_env$routes %||%
      getOption("aisdk.capability_models", list())
  )
}

#' @keywords internal
store_capability_model_routes <- function(routes = list()) {
  routes <- normalize_capability_model_routes(routes)
  .capability_model_env$routes <- routes
  options(aisdk.capability_models = routes)
  invisible(routes)
}

#' @keywords internal
capability_model_label <- function(model) {
  label <- default_model_id(model)
  if (!is.null(label) && nzchar(label)) {
    return(label)
  }
  if (is.character(model) && length(model) > 0) {
    return(model[[1]])
  }
  paste(class(model), collapse = "/")
}

#' @title Set Capability Model
#' @description
#' Set the model used for a named capability. For example, the default chat
#' model can remain a low-cost text model while `vision.inspect` uses a
#' vision-capable language model and `image.generate` uses an image model.
#' @param capability Capability route name, such as `"vision.inspect"` or a
#'   named list of routes for batch updates.
#' @param model Model ID string or model object. Passing `NULL` clears the
#'   route for `capability`.
#' @param type Model type for this route: `"auto"`, `"language"`,
#'   `"embedding"`, or `"image"`.
#' @param required_model_capabilities Optional model capability flags required
#'   by this route, such as `"vision_input"`.
#' @return Invisibly returns the previous model for the route, or the previous
#'   route list for batch updates.
#' @export
#' @examples
#' old <- set_capability_model("vision.inspect", "openai:gpt-4o")
#' get_capability_model("vision.inspect")
#' set_capability_model("vision.inspect", old)
set_capability_model <- function(capability,
                                 model,
                                 type = "auto",
                                 required_model_capabilities = NULL) {
  if (is.list(capability) && missing(model)) {
    old <- get_capability_model_routes()
    routes <- old
    updates <- normalize_capability_model_routes(capability)
    for (name in names(updates)) {
      routes[[name]] <- updates[[name]]
    }
    store_capability_model_routes(routes)
    return(invisible(old))
  }

  capability <- normalize_capability_name(capability)
  old <- get_capability_model(capability, default = NULL)

  if (missing(model)) {
    rlang::abort("`model` is required.")
  }
  if (is.null(model)) {
    routes <- get_capability_model_routes()
    routes[[capability]] <- NULL
    store_capability_model_routes(routes)
    return(invisible(old))
  }

  routes <- get_capability_model_routes()
  routes[[capability]] <- create_capability_model_route(
    model = model,
    type = type,
    required_model_capabilities = required_model_capabilities
  )
  store_capability_model_routes(routes)
  invisible(old)
}

#' @title Get Capability Model
#' @description Return the configured model for a capability route.
#' @param capability Capability route name.
#' @param default Value returned when no route is configured.
#' @return A model ID string, model object, or `default`.
#' @export
get_capability_model <- function(capability, default = NULL) {
  capability <- normalize_capability_name(capability)
  route <- get_capability_model_routes()[[capability]]
  if (is.null(route)) {
    return(default)
  }
  route$model %||% default
}

#' @title List Capability Models
#' @description List configured capability model routes.
#' @return A data frame with capability route names and model settings.
#' @export
list_capability_models <- function() {
  routes <- get_capability_model_routes()
  if (length(routes) == 0) {
    return(data.frame(
      capability = character(0),
      model = character(0),
      type = character(0),
      required_model_capabilities = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    capability = names(routes),
    model = vapply(routes, function(route) capability_model_label(route$model), character(1)),
    type = vapply(routes, function(route) route$type %||% "auto", character(1)),
    required_model_capabilities = vapply(
      routes,
      function(route) paste(route$required_model_capabilities %||% character(0), collapse = ", "),
      character(1)
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' @title Clear Capability Model
#' @description Clear one or all configured capability model routes.
#' @param capability Optional capability route name. If `NULL`, all routes are
#'   cleared.
#' @return Invisibly returns the previous route list.
#' @export
clear_capability_model <- function(capability = NULL) {
  old <- get_capability_model_routes()
  if (is.null(capability)) {
    store_capability_model_routes(list())
    return(invisible(old))
  }

  routes <- old
  for (name in as.character(capability)) {
    routes[[normalize_capability_name(name)]] <- NULL
  }
  store_capability_model_routes(routes)
  invisible(old)
}

#' @keywords internal
get_session_capability_model_routes <- function(session = NULL) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(list())
  }
  normalize_capability_model_routes(session$get_metadata("capability_models", list()))
}

#' @keywords internal
select_model_ref_for_capability <- function(capability,
                                            explicit_model = NULL,
                                            session = NULL,
                                            fallback_model = NULL,
                                            default_model = NULL) {
  capability <- normalize_capability_name(capability)

  if (!is.null(explicit_model) && is.character(explicit_model) && length(explicit_model) == 1) {
    explicit_model <- trimws(explicit_model)
    if (!nzchar(explicit_model)) {
      explicit_model <- NULL
    }
  }
  if (!is.null(explicit_model)) {
    return(list(
      model = normalize_model_ref(explicit_model, arg = "`explicit_model`"),
      route = NULL,
      source = "explicit"
    ))
  }

  session_routes <- get_session_capability_model_routes(session)
  if (!is.null(session_routes[[capability]])) {
    return(list(
      model = session_routes[[capability]]$model,
      route = session_routes[[capability]],
      source = "session"
    ))
  }

  global_routes <- get_capability_model_routes()
  if (!is.null(global_routes[[capability]])) {
    return(list(
      model = global_routes[[capability]]$model,
      route = global_routes[[capability]],
      source = "global"
    ))
  }

  if (!is.null(fallback_model) && is.character(fallback_model) && length(fallback_model) == 1) {
    fallback_model <- trimws(fallback_model)
    if (!nzchar(fallback_model)) {
      fallback_model <- NULL
    }
  }
  if (!is.null(fallback_model)) {
    return(list(
      model = normalize_model_ref(fallback_model, arg = "`fallback_model`"),
      route = NULL,
      source = "fallback"
    ))
  }

  if (!is.null(default_model)) {
    return(list(
      model = normalize_model_ref(default_model, arg = "`default_model`"),
      route = NULL,
      source = "default"
    ))
  }

  list(model = NULL, route = NULL, source = "missing")
}

#' @keywords internal
model_ref_capability_value <- function(model, capability) {
  if (inherits(model, "LanguageModelV1")) {
    model <- enrich_language_model_capabilities(model)
  }
  if ((inherits(model, "LanguageModelV1") || inherits(model, "ImageModelV1")) &&
      is.list(model$capabilities)) {
    return(model$capabilities[[capability]] %||% NULL)
  }

  if (!is.character(model) || length(model) == 0 || !nzchar(model[[1]])) {
    return(NULL)
  }

  model_id <- model[[1]]
  sep_pos <- regexpr(":", model_id, fixed = TRUE)
  if (sep_pos < 1) {
    return(NULL)
  }

  provider <- substr(model_id, 1, sep_pos - 1)
  provider_model <- substr(model_id, sep_pos + 1, nchar(model_id))
  info <- tryCatch(
    get_model_info(provider, provider_model),
    error = function(e) NULL
  )
  caps <- info$capabilities %||% list()
  caps[[capability]] %||% NULL
}

#' @keywords internal
model_ref_capability_explicitly_unavailable <- function(model, capability) {
  identical(model_ref_capability_value(model, capability), FALSE)
}

#' @keywords internal
validate_model_ref_capabilities <- function(model,
                                            required_model_capabilities = NULL,
                                            capability = NULL) {
  required_model_capabilities <- normalize_required_model_capabilities(required_model_capabilities)
  if (length(required_model_capabilities) == 0 || is.null(model)) {
    return(invisible(TRUE))
  }

  missing <- required_model_capabilities[vapply(
    required_model_capabilities,
    function(cap) model_ref_capability_explicitly_unavailable(model, cap),
    logical(1)
  )]
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  route_text <- if (!is.null(capability)) {
    paste0(" for capability `", capability, "`")
  } else {
    ""
  }
  rlang::abort(paste0(
    "Model `", capability_model_label(model), "`", route_text,
    " does not advertise required model capability: ",
    paste(missing, collapse = ", "), "."
  ))
}

#' @title Resolve Model For Capability
#' @description
#' Resolve and validate the model selected for a capability route.
#' @param capability Capability route name.
#' @param explicit_model Optional explicit model override.
#' @param type Expected model type: `"language"`, `"embedding"`, or `"image"`.
#' @param required_model_capabilities Optional required capability flags.
#' @param session Optional `ChatSession`; session routes override global routes.
#' @param registry Optional provider registry.
#' @param fallback_model Optional model used when no route is configured.
#' @param default_model Optional final fallback. If omitted for language models,
#'   the package default model is used.
#' @return A resolved model object.
#' @export
resolve_model_for_capability <- function(capability,
                                         explicit_model = NULL,
                                         type = c("language", "embedding", "image"),
                                         required_model_capabilities = NULL,
                                         session = NULL,
                                         registry = NULL,
                                         fallback_model = NULL,
                                         default_model = NULL) {
  capability <- normalize_capability_name(capability)
  type <- match.arg(type)
  if (is.null(default_model) && identical(type, "language")) {
    default_model <- get_model()
  }

  selected <- select_model_ref_for_capability(
    capability = capability,
    explicit_model = explicit_model,
    session = session,
    fallback_model = fallback_model,
    default_model = default_model
  )

  if (is.null(selected$model)) {
    rlang::abort(paste0(
      "No model configured for capability `", capability,
      "`. Supply `explicit_model`, set a capability model route, or provide `fallback_model`."
    ))
  }

  route_type <- selected$route$type %||% "auto"
  if (!identical(route_type, "auto") && !identical(route_type, type)) {
    rlang::abort(paste0(
      "Capability `", capability, "` is configured as type `", route_type,
      "`, but `", type, "` was requested."
    ))
  }

  required <- unique(c(
    normalize_required_model_capabilities(selected$route$required_model_capabilities %||% NULL),
    normalize_required_model_capabilities(required_model_capabilities)
  ))

  validate_model_ref_capabilities(selected$model, required, capability = capability)
  model <- resolve_model(selected$model, registry = registry, type = type)
  validate_model_ref_capabilities(model, required, capability = capability)
  model
}
