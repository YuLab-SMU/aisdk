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
    moonshot = list(
        base_url = "https://api.moonshot.cn/v1",
        env_key = "MOONSHOT_API_KEY"
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
    fetched_ids <- vapply(data$data, function(m) m$id %||% "", character(1))
    existing_ids <- names(existing)
    new_ids <- setdiff(fetched_ids, existing_ids)
    removed_ids <- setdiff(existing_ids, fetched_ids)
    preserved_count <- length(intersect(fetched_ids, existing_ids))

    models <- lapply(data$data, function(m) {
        id <- m$id
        old <- existing[[id]]

        # Build new entry, merging with existing enriched data
        new_entry <- list(id = id)

        # Preserve or auto-generate description
        new_entry$description <- if (!is.null(old$description)) old$description else paste0("Model: ", id)

        # Preserve type and family
        if (!is.null(old$type)) new_entry$type <- old$type
        if (!is.null(old$family)) new_entry$family <- old$family

        # Preserve existing capabilities entirely if they exist
        if (!is.null(old$capabilities)) {
            new_entry$capabilities <- old$capabilities
        }

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

    change_log <- sprintf(
        "new: %d, removed: %d, preserved: %d",
        length(new_ids), length(removed_ids), preserved_count
    )
    message(sprintf(
        "Updated %d models for '%s' -> %s (change_log: %s)",
        length(models), provider, file_path, change_log
    ))
    invisible(TRUE)
}
