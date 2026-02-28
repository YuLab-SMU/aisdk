#' @name utils_models_update
#' @title Model Synchronization Utilities
#' @description
#' Internal utilities for synchronizing model configurations with provider APIs.
#' When re-syncing, manually enriched fields (pricing, context, capabilities, etc.)
#' are preserved and merged back into the updated file.
#' @keywords internal
NULL

# Known default base URLs for providers
.provider_defaults <- list(
    stepfun = list(
        base_url = "https://api.stepfun.com/v1",
        env_key = "STEPFUN_API_KEY"
    ),
    openai = list(
        base_url = "https://api.openai.com/v1",
        env_key = "OPENAI_API_KEY"
    ),
    volcengine = list(
        base_url = "https://ark.cn-beijing.volces.com/api/v3",
        env_key = "ARK_API_KEY"
    ),
    deepseek = list(
        base_url = "https://api.deepseek.com/v1",
        env_key = "DEEPSEEK_API_KEY"
    ),
    xai = list(
        base_url = "https://api.x.ai/v1",
        env_key = "XAI_API_KEY"
    )
)

#' @title Update Provider Models
#' @description
#' Fetches the model list from a provider's API and updates the local JSON config.
#' Manually enriched metadata (pricing, context, capabilities, family, etc.) is preserved
#' during re-sync: only the ID list is refreshed, existing enriched data is merged back.
#'
#' @param provider The name of the provider (e.g., "stepfun").
#' @return Invisible TRUE on success.
#' @export
#' @keywords internal
update_provider_models <- function(provider) {
    # --- Resolve API configuration ---
    defaults <- .provider_defaults[[provider]]
    env_prefix <- toupper(provider)

    env_key_name <- if (!is.null(defaults)) defaults$env_key else sprintf("%s_API_KEY", env_prefix)
    api_key <- Sys.getenv(env_key_name)
    base_url <- Sys.getenv(sprintf("%s_BASE_URL", env_prefix))

    if (base_url == "" && !is.null(defaults)) {
        base_url <- defaults$base_url
    }

    if (api_key == "") {
        rlang::abort(sprintf("API key not found. Please set %s environment variable.", env_key_name))
    }

    endpoint <- paste0(sub("/$", "", base_url), "/models")

    # --- Fetch models from API ---
    req <- httr2::request(endpoint) |>
        httr2::req_headers(Authorization = paste("Bearer", api_key))

    resp <- httr2::req_perform(req)
    data <- httr2::resp_body_json(resp)

    if (is.null(data$data)) {
        rlang::abort(sprintf("Failed to parse /models response from %s.", provider))
    }

    # --- Load existing config for merge ---
    config_dir <- "inst/extdata/models"
    if (!dir.exists(config_dir)) {
        dir.create(config_dir, recursive = TRUE)
    }
    file_path <- file.path(config_dir, paste0(provider, ".json"))

    existing <- list()
    if (file.exists(file_path)) {
        existing <- jsonlite::read_json(file_path)
        names(existing) <- vapply(existing, function(m) m$id %||% "", character(1))
    }

    # --- Process and merge ---
    models <- lapply(data$data, function(m) {
        id <- m$id
        old <- existing[[id]]

        # Auto-infer basic capabilities from model ID
        is_vision <- grepl("v|-v|-vision|vision", tolower(id))
        is_reasoning <- grepl("o1|o3|r1|reason|think", tolower(id))
        is_audio <- grepl("audio|asr|tts", tolower(id))

        # Infer type
        inferred_type <- if (is_audio) "audio" else "language"

        # Build new entry, merging with existing enriched data
        new_entry <- list(id = id)

        # Preserve or infer type
        new_entry$type <- if (!is.null(old$type)) old$type else inferred_type

        # Preserve or auto-generate description
        new_entry$description <- if (!is.null(old$description)) old$description else paste0("Model: ", id)

        # Preserve family
        if (!is.null(old$family)) new_entry$family <- old$family

        # Merge capabilities: preserve existing, fill gaps with inference
        old_caps <- old$capabilities %||% list()
        new_entry$capabilities <- list(
            reasoning = old_caps$reasoning %||% is_reasoning,
            vision = old_caps$vision %||% is_vision,
            audio_input = old_caps$audio_input %||% is_audio,
            function_call = old_caps$function_call %||% NULL,
            structured_output = old_caps$structured_output %||% NULL,
            search = old_caps$search %||% NULL
        )
        # Remove NULL entries from capabilities
        new_entry$capabilities <- Filter(Negate(is.null), new_entry$capabilities)

        # Preserve enriched fields entirely if they exist
        if (!is.null(old$context)) new_entry$context <- old$context
        if (!is.null(old$pricing)) new_entry$pricing <- old$pricing
        if (!is.null(old$rate_limits)) new_entry$rate_limits <- old$rate_limits
        if (!is.null(old$input_types)) new_entry$input_types <- old$input_types
        if (!is.null(old$output_types)) new_entry$output_types <- old$output_types

        new_entry
    })

    # --- Write ---
    json_data <- jsonlite::toJSON(models, auto_unbox = TRUE, pretty = TRUE)
    writeLines(json_data, file_path)

    message(sprintf(
        "Updated %d models for '%s' -> %s (merged %d existing entries)",
        length(models), provider, file_path, length(existing)
    ))
    invisible(TRUE)
}
