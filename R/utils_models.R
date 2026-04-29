#' @name utils_models
#' @title Model List Management Utilities
#' @description
#' Utilities for loading, querying, and formatting provider model lists from static configuration.
#' @keywords internal
NULL

#' @title Load Models Configuration
#' @description
#' Internal helper to load the `models` JSON files.
#' @return A list representing the parsed JSON of models per provider.
#' @keywords internal
load_models_config <- function() {
    config_dir <- find_source_models_config_dir()

    if (is.null(config_dir)) {
        config_dir <- system.file("extdata", "models", package = "aisdk")
    }

    if (config_dir == "" || !dir.exists(config_dir)) {
        return(list())
    }

    json_files <- list.files(config_dir, pattern = "\\.json$", full.names = TRUE)
    config <- list()
    for (file in json_files) {
        provider_name <- tools::file_path_sans_ext(basename(file))
        config[[provider_name]] <- tryCatch(
            jsonlite::read_json(file),
            error = function(e) {
                NULL
            }
        )
    }

    config
}

find_source_models_config_dir <- function(start = getwd()) {
    current <- normalizePath(start, mustWork = FALSE)

    repeat {
        desc_path <- file.path(current, "DESCRIPTION")
        config_dir <- file.path(current, "inst", "extdata", "models")

        if (file.exists(desc_path) && dir.exists(config_dir)) {
            package <- tryCatch(
                read.dcf(desc_path, fields = "Package")[[1]],
                error = function(e) NA_character_
            )
            if (identical(package, "aisdk")) {
                return(config_dir)
            }
        }

        parent <- dirname(current)
        if (identical(parent, current)) {
            break
        }
        current <- parent
    }

    NULL
}

# --- Helper to safely extract nested fields ---
.safe <- function(x, default = NA) {
    if (is.null(x)) default else x
}

#' @title List Models for Provider
#' @description
#' Returns a data frame of models for the specified provider based on the static configuration.
#' Includes enriched metadata when available (context window, pricing, capabilities).
#' @param provider The name of the provider (e.g., "stepfun"). If NULL, all providers are listed.
#' @return A data frame containing model details.
#' @export
list_models <- function(provider = NULL) {
    config <- load_models_config()
    if (length(config) == 0) {
        return(data.frame())
    }

    result <- list()
    providers_to_check <- if (is.null(provider)) names(config) else provider

    for (p in providers_to_check) {
        if (!is.null(config[[p]])) {
            models <- config[[p]]
            df <- do.call(rbind, lapply(models, function(m) {
                caps <- m$capabilities %||% list()
                ctx <- m$context %||% list()
                price <- m$pricing %||% list()

                data.frame(
                    provider = p,
                    id = .safe(m$id, NA_character_),
                    type = .safe(m$type, NA_character_),
                    family = .safe(m$family, NA_character_),
                    description = .safe(m$description, NA_character_),
                    reasoning = .safe(caps$reasoning, .safe(m$reasoning, FALSE)),
                    vision_input = .safe(caps$vision_input, .safe(caps$vision, .safe(m$vision, FALSE))),
                    image_output = .safe(caps$image_output, FALSE),
                    image_edit = .safe(caps$image_edit, FALSE),
                    audio_input = .safe(caps$audio_input, FALSE),
                    audio_output = .safe(caps$audio_output, FALSE),
                    function_call = .safe(caps$function_call, NA),
                    structured_output = .safe(caps$structured_output, FALSE),
                    web_search = .safe(caps$web_search, FALSE),
                    context_window = .safe(ctx$context_window, NA_integer_),
                    max_output = .safe(ctx$max_output_tokens, NA_integer_),
                    input_price = .safe(price$input, NA_real_),
                    output_price = .safe(price$output, NA_real_),
                    stringsAsFactors = FALSE
                )
            }))
            result[[p]] <- df
        }
    }

    if (length(result) == 0) {
        return(data.frame())
    }

    df <- do.call(rbind, result)
    rownames(df) <- NULL
    df
}

#' @title Get Full Model Info
#' @description
#' Returns the full metadata for a single model as a list.
#' Useful for framework internals to auto-configure parameters
#' (e.g., max_tokens, context_window).
#' @param provider The name of the provider.
#' @param model_id The model ID string.
#' @return A list containing all available metadata for the model, or NULL if not found.
#' @export
get_model_info <- function(provider, model_id) {
    config <- load_models_config()
    models <- config[[provider]]
    if (is.null(models)) {
        return(NULL)
    }

    for (m in models) {
        if (identical(m$id, model_id)) {
            return(m)
        }
    }
    NULL
}

#' @title Generate Document Strings for Models
#' @description
#' Helper for roxygen2 `@eval` tag to dynamically insert supported models into documentation.
#' Shows enriched information (context window, capabilities) when available.
#' @param provider The name of the provider.
#' @param max_items Maximum number of models to display. Defaults to 15.
#' @return A string containing roxygen-formatted documentation of the models.
#' @export
#' @keywords internal
generate_model_docs <- function(provider, max_items = 15) {
    config <- load_models_config()

    if (is.null(config[[provider]])) {
        return(c("@section Supported Models:", "No models documented for this provider."))
    }

    models <- config[[provider]]
    total <- length(models)
    truncated <- total > max_items
    display_models <- if (truncated) models[seq_len(max_items)] else models

    lines <- c("@section Supported Models:", "\\itemize{")
    for (m in display_models) {
        # Build capability tags
        caps <- m$capabilities %||% list()
        tags <- c()
        if (isTRUE(caps$reasoning) || isTRUE(m$reasoning)) tags <- c(tags, "Reasoning")
        if (isTRUE(caps$vision_input) || isTRUE(caps$vision) || isTRUE(m$vision)) tags <- c(tags, "Vision")
        if (isTRUE(caps$function_call)) tags <- c(tags, "Tools")
        if (isTRUE(caps$audio_input) || isTRUE(caps$audio_output)) tags <- c(tags, "Audio")
        if (isTRUE(caps$image_output)) tags <- c(tags, "Image-Out")
        if (isTRUE(caps$image_edit)) tags <- c(tags, "Image-Edit")
        if (isTRUE(caps$structured_output)) tags <- c(tags, "Structured")
        if (isTRUE(caps$web_search)) tags <- c(tags, "Search")

        tag_str <- if (length(tags) > 0) sprintf(" (%s)", paste(tags, collapse = ", ")) else ""

        # Build context info
        ctx <- m$context %||% list()
        ctx_str <- ""
        if (!is.null(ctx$context_window)) {
            ctx_k <- paste0(round(ctx$context_window / 1000), "k")
            ctx_str <- sprintf(" | ctx: %s", ctx_k)
        }

        description <- m$description %||% ""

        line <- sprintf("  \\item \\strong{%s}: %s%s%s", m$id, description, tag_str, ctx_str)
        lines <- c(lines, line)
    }

    if (truncated) {
        remaining <- total - max_items
        lines <- c(lines, sprintf(
            "  \\item \\emph{... and %d more models. Use \\code{list_models(\"%s\")} to see all.}",
            remaining, provider
        ))
    }

    lines <- c(lines, "}")
    return(lines)
}
