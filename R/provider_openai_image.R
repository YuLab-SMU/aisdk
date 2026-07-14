#' @title OpenAI Image Model
#' @description
#' Image model implementation for OpenAI's image generation and editing APIs.
#' Exported so that OpenAI-compatible providers in companion packages
#' (e.g. \pkg{aisdk.providers}) can construct image models across package
#' boundaries.
#' @keywords internal
#' @export
OpenAIImageModel <- R6::R6Class(
  "OpenAIImageModel",
  inherit = ImageModelV1,
  private = list(
    config = NULL,
    last_response_id = NULL,
    # Mirror the Responses language model: only chain image-edit turns
    # server-side when the configured state mode allows it (default "auto"
    # for first-party `create_openai()`, "stateless" for custom proxies).
    use_server_state = function() {
      mode <- responses_normalize_state_mode(private$config$responses_state_mode)
      mode %in% c("server", "auto")
    },
    get_headers = function(include_content_type = TRUE) {
      h <- list()
      if (nzchar(private$config$api_key %||% "")) {
        h$Authorization <- paste("Bearer", private$config$api_key)
      }
      if (include_content_type) {
        h$`Content-Type` <- "application/json"
      }
      if (!is.null(private$config$organization)) {
        h$`OpenAI-Organization` <- private$config$organization
      }
      if (!is.null(private$config$headers)) {
        h <- c(h, private$config$headers)
      }
      h
    },
    supports_image_response_format = function() {
      provider_name <- tolower(private$config$provider_name %||% "openai")
      base_url <- tolower(private$config$base_url %||% "")
      if (identical(provider_name, "aihubmix")) {
        return(FALSE)
      }
      if (grepl("aihubmix\\.com", base_url)) {
        return(FALSE)
      }
      TRUE
    },
    is_aihubmix_compatible = function() {
      provider_name <- tolower(private$config$provider_name %||% "openai")
      base_url <- tolower(private$config$base_url %||% "")
      identical(provider_name, "aihubmix") || grepl("aihubmix\\.com", base_url)
    },
    aihubmix_size_from_dimensions = function(width = NULL, height = NULL) {
      width <- suppressWarnings(as.numeric(width))
      height <- suppressWarnings(as.numeric(height))
      if (
        length(width) != 1 || length(height) != 1 ||
          is.na(width) || is.na(height) ||
          width <= 0 || height <= 0
      ) {
        return(NULL)
      }

      ratio <- width / height
      if (ratio > 1.15) {
        return("1536x1024")
      }
      if (ratio < 0.87) {
        return("1024x1536")
      }
      "1024x1024"
    },
    normalize_aihubmix_generation_params = function(params) {
      if (!private$is_aihubmix_compatible()) {
        return(params)
      }

      if (is.null(params$size)) {
        params$size <- private$aihubmix_size_from_dimensions(
          width = params$width %||% NULL,
          height = params$height %||% NULL
        )
      }

      if (!is.null(params$size) && identical(tolower(as.character(params$size)[[1]]), "auto")) {
        params$size <- NULL
      }

      if (!is.null(params$transparent_background) && is.null(params$background)) {
        params$background <- if (isTRUE(params$transparent_background)) "transparent" else "opaque"
      }

      params$width <- NULL
      params$height <- NULL
      params$transparent_background <- NULL
      params
    },
    is_gpt_image_2 = function() {
      grepl("(^|/)gpt-image-2($|-)", self$model_id %||% "", perl = TRUE)
    },
    is_gpt_image_family = function() {
      grepl("(^|/)(gpt-image-2|gpt-image-1(\\.5|-mini)?|chatgpt-image-latest)($|-)", self$model_id %||% "", perl = TRUE)
    },
    validate_image_params = function(params, request_type = c("generate", "edit")) {
      request_type <- match.arg(request_type)

      if (!is.null(params$output_compression)) {
        compression <- suppressWarnings(as.numeric(params$output_compression))
        if (is.na(compression) || compression < 0 || compression > 100) {
          rlang::abort("`output_compression` must be a number between 0 and 100.")
        }
        fmt <- tolower(params$output_format %||% "")
        if (!fmt %in% c("jpeg", "jpg", "webp")) {
          rlang::abort("`output_compression` requires `output_format = 'jpeg'` or `output_format = 'webp'`.")
        }
      }

      if (!is.null(params$input_fidelity)) {
        if (request_type != "edit") {
          rlang::abort("`input_fidelity` is only supported for image editing workflows.")
        }
        if (private$is_gpt_image_2()) {
          rlang::abort("`input_fidelity` is fixed for `gpt-image-2` and cannot be overridden.")
        }
      }

      if (!is.null(params$response_format) && private$is_gpt_image_family()) {
        fmt <- tolower(as.character(params$response_format)[[1]])
        if (identical(fmt, "url")) {
          rlang::abort("GPT image models return base64 image payloads; `response_format = 'url'` is not supported.")
        }
      }

      invisible(TRUE)
    },
    build_generation_body = function(params) {
      params <- private$normalize_aihubmix_generation_params(params)
      private$validate_image_params(params, request_type = "generate")

      body <- list(
        model = self$model_id,
        prompt = params$prompt
      )
      if (private$supports_image_response_format()) {
        body$response_format <- params$response_format %||% "b64_json"
      }

      if (!is.null(params$n)) body$n <- params$n
      if (!is.null(params$size)) body$size <- params$size
      if (!is.null(params$quality)) body$quality <- params$quality
      if (!is.null(params$background)) body$background <- params$background
      if (!is.null(params$moderation)) body$moderation <- params$moderation
      if (!is.null(params$output_format)) body$output_format <- params$output_format
      if (!is.null(params$output_compression)) body$output_compression <- params$output_compression

      handled <- c(
        "prompt", "output_dir", "response_format", "n", "size", "quality",
        "background", "moderation", "output_format", "output_compression",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra <- params[setdiff(names(params), handled)]
      if (length(extra) > 0) {
        body <- utils::modifyList(body, extra)
      }

      body[!sapply(body, is.null)]
    },
    build_responses_tool_config = function(params) {
      # Build the `image_generation` tool config block for the Responses-API
      # fallback. Mirrors the field subset documented for the tool; keeps the
      # same normalize + validate sequence as build_generation_body so behavior
      # is consistent across the two paths.
      params <- private$normalize_aihubmix_generation_params(params)
      private$validate_image_params(params, request_type = "generate")

      tool <- list(
        type = "image_generation",
        model = params$image_model %||% self$model_id
      )
      if (!is.null(params$quality))            tool$quality <- params$quality
      if (!is.null(params$size))               tool$size <- params$size
      if (!is.null(params$output_format))      tool$output_format <- params$output_format
      if (!is.null(params$output_compression)) tool$output_compression <- params$output_compression
      if (!is.null(params$background))         tool$background <- params$background
      if (!is.null(params$moderation))         tool$moderation <- params$moderation
      if (!is.null(params$n))                  tool$n <- params$n

      tool[!sapply(tool, is.null)]
    },
    build_responses_edit_tool_config = function(params, mask_data_url = NULL) {
      # Edit-flavored variant of build_responses_tool_config: sets
      # action = "edit" explicitly (the tool's `auto` default also works,
      # but explicit is safer), forwards `input_fidelity` (only allowed for
      # edits per validate_image_params), and adds `input_image_mask` when a
      # mask was supplied. The source image goes into the request `input`
      # array as an input_image block — not into the tool config.
      private$validate_image_params(params, request_type = "edit")

      tool <- list(
        type = "image_generation",
        action = "edit",
        model = params$image_model %||% self$model_id
      )
      if (!is.null(params$quality))            tool$quality <- params$quality
      if (!is.null(params$size))               tool$size <- params$size
      if (!is.null(params$output_format))      tool$output_format <- params$output_format
      if (!is.null(params$output_compression)) tool$output_compression <- params$output_compression
      if (!is.null(params$background))         tool$background <- params$background
      if (!is.null(params$moderation))         tool$moderation <- params$moderation
      if (!is.null(params$input_fidelity))     tool$input_fidelity <- params$input_fidelity
      if (!is.null(mask_data_url)) {
        tool$input_image_mask <- list(image_url = mask_data_url)
      }

      tool[!sapply(tool, is.null)]
    },
    build_edit_body = function(params) {
      upload_dir <- params$output_dir %||% tempdir()
      private$validate_image_params(params, request_type = "edit")
      image_inputs <- coerce_image_inputs(params$image)
      image_paths <- lapply(seq_along(image_inputs), function(i) {
        materialize_image_upload(
          image_inputs[[i]],
          output_dir = upload_dir,
          prefix = sprintf("openai_image_%02d", i)
        )
      })

      body <- list(
        model = self$model_id,
        prompt = params$prompt %||% "Edit this image."
      )
      if (private$supports_image_response_format()) {
        body$response_format <- params$response_format %||% "b64_json"
      }

      if (length(image_paths) == 1) {
        body$image <- curl::form_file(image_paths[[1]])
      } else {
        body <- c(
          body,
          stats::setNames(lapply(image_paths, curl::form_file), rep("image[]", length(image_paths)))
        )
      }

      if (!is.null(params$mask)) {
        mask_path <- materialize_image_upload(params$mask, output_dir = upload_dir, prefix = "openai_mask")
        body$mask <- curl::form_file(mask_path)
      }

      if (!is.null(params$n)) body$n <- as.character(params$n)
      if (!is.null(params$size)) body$size <- params$size
      if (!is.null(params$quality)) body$quality <- params$quality
      if (!is.null(params$background)) body$background <- params$background
      if (!is.null(params$output_format)) body$output_format <- params$output_format
      if (!is.null(params$output_compression)) body$output_compression <- params$output_compression
      if (!is.null(params$input_fidelity)) body$input_fidelity <- params$input_fidelity

      handled <- c(
        "image", "mask", "prompt", "output_dir", "response_format", "n",
        "size", "quality", "background", "output_format", "output_compression",
        "input_fidelity",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra <- params[setdiff(names(params), handled)]
      if (length(extra) > 0) {
        body <- c(body, extra)
      }

      body[!sapply(body, is.null)]
    },
    parse_image_response = function(response,
                                    output_dir = tempdir(),
                                    prefix = "openai_image",
                                    requested_output_format = NULL) {
      images <- list()

      if (!is.null(response$data) && length(response$data) > 0) {
        for (item in response$data) {
          artifact <- list(
            revised_prompt = item$revised_prompt %||% NULL
          )

          if (!is.null(item$b64_json)) {
            artifact$bytes <- base64enc::base64decode(item$b64_json)
            artifact$media_type <- switch(item$output_format %||% requested_output_format %||% "",
              png = "image/png",
              jpeg = "image/jpeg",
              jpg = "image/jpeg",
              webp = "image/webp",
              "image/png"
            )
          } else if (!is.null(item$url)) {
            artifact$uri <- item$url
          }

          images <- c(images, list(artifact))
        }
      }

      finalize_image_artifacts(images, output_dir = output_dir, prefix = prefix)
    }
  ),
  public = list(
    #' @description Initialize the OpenAI image model.
    #' @param model_id The model ID (e.g., "gpt-image-2", "gpt-image-1.5").
    #' @param config Configuration list.
    initialize = function(model_id, config) {
      super$initialize(
        provider = config$provider_name %||% "openai",
        model_id = model_id,
        capabilities = list(
          image_output = TRUE,
          image_edit = TRUE
        )
      )
      private$config <- config
    },

    #' @description Generate images.
    #'
    #' Tries the classic `POST /v1/images/generations` endpoint first. If that
    #' returns a 404 with `invalid_api_path` / "not available" — the signal
    #' some OpenAI-compatible proxies emit when they only expose the newer
    #' Responses API — falls back to `POST /v1/responses` with the
    #' `image_generation` tool and decodes the returned base64 image.
    #'
    #' On the fallback path, the standard image params (`quality`, `size`,
    #' `output_format`, `output_compression`, `background`, `moderation`, `n`)
    #' are forwarded into the tool config, and a `previous_response_id` from a
    #' prior fallback call is auto-attached so multi-turn edits ("now make it
    #' realistic") work the same as on the language-model path. Use
    #' `get_last_response_id()` / `reset()` to inspect or clear that state.
    #'
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_generate_image = function(params) {
      if (is.null(params$prompt) || !nzchar(params$prompt)) {
        rlang::abort("`prompt` must be a non-empty string.")
      }

      classic <- tryCatch(
        self$do_generate_image_classic(params),
        error = function(e) e
      )
      if (!inherits(classic, "error")) {
        return(classic)
      }

      if (self$looks_like_missing_classic_endpoint(classic)) {
        message(
          "OpenAI image generation: classic /v1/images/generations is unreachable on this endpoint. ",
          "Falling back to /v1/responses with the `image_generation` tool."
        )
        return(self$do_generate_image_via_responses(params))
      }

      stop(classic)
    },

    #' @description Generate images via the classic `POST /v1/images/generations`
    #'   endpoint. Called by `do_generate_image()`; exposed for callers that want
    #'   to bypass the Responses-API fallback on proxies they trust.
    #' @param params A list of call options (see `do_generate_image`).
    #' @return A GenerateImageResult object.
    do_generate_image_classic = function(params) {
      url <- api_endpoint_urls(private$config, "/images/generations")
      headers <- private$get_headers(include_content_type = TRUE)
      body <- private$build_generation_body(params)
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      GenerateImageResult$new(
        images = private$parse_image_response(
          response,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image",
          requested_output_format = body$output_format %||% NULL
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Generate images via `POST /v1/responses` with the
    #'   `image_generation` tool. Used as a fallback when the classic
    #'   `/v1/images/generations` endpoint is unreachable (e.g. OpenAI-compatible
    #'   proxies that only expose the Responses API).
    #' @param params A list of call options (see `do_generate_image`).
    #' @return A GenerateImageResult object.
    do_generate_image_via_responses = function(params) {
      url <- api_endpoint_urls(private$config, "/responses")
      # Force identity transfer-encoding: some OpenAI-compatible proxies
      # advertise gzip but send a malformed Content-Encoding header on the
      # /v1/responses route. With `Accept-Encoding: identity` the proxy
      # streams uncompressed and httr2 parses cleanly.
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )
      body <- list(
        model = self$model_id,
        input = params$prompt,
        tools = list(private$build_responses_tool_config(params))
      )
      if (private$use_server_state() && !is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      # Record the conversation id even before parsing output, so a callback
      # like "now make it realistic" can chain off this turn even if the
      # current response yielded no image.
      if (!is.null(response$id)) {
        private$last_response_id <- response$id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      images <- list()
      for (item in response$output %||% list()) {
        if (identical(item$type %||% "", "image_generation_call") && !is.null(item$result)) {
          images <- c(images, list(list(
            bytes = base64enc::base64decode(item$result),
            media_type = media_type,
            revised_prompt = item$revised_prompt %||% NULL
          )))
        }
      }

      if (!length(images)) {
        rlang::abort(c(
          "Responses API returned no `image_generation_call` output.",
          i = "The proxy accepted the request but produced no image; this is usually a model or prompt issue."
        ))
      }

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          images,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image_responses"
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Heuristic check used by `do_generate_image()` /
    #'   `do_edit_image()` to decide whether a classic-endpoint error looks
    #'   like "endpoint not available" on the proxy, in which case the
    #'   Responses-API fallback is taken. Matches both the `/images/generations`
    #'   and `/images/edits` paths.
    #' @param err An error condition raised by the classic-endpoint call.
    #' @return `TRUE` if the error message matches the "missing endpoint" shape.
    looks_like_missing_classic_endpoint = function(err) {
      msg <- conditionMessage(err) %||% ""
      isTRUE(grepl("404", msg, fixed = TRUE)) &&
        (grepl("invalid_api_path", msg, fixed = TRUE) ||
         grepl("not available", msg, fixed = TRUE) ||
         grepl("images/(generations|edits)", msg))
    },

    #' @description Return the most recent Responses-API response id captured
    #' during the `/v1/responses` fallback path. Used to chain multi-turn
    #' image edits via `previous_response_id`.
    #' @return Character scalar or `NULL` if no fallback call has succeeded yet.
    get_last_response_id = function() {
      private$last_response_id
    },

    #' @description Clear any stored `previous_response_id`, ending the current
    #' multi-turn image session on the Responses-API fallback path.
    #' @return The model, invisibly.
    reset = function() {
      private$last_response_id <- NULL
      invisible(self)
    },

    #' @description Edit images.
    #'
    #' Tries the classic `POST /v1/images/edits` multipart endpoint first.
    #' If that returns the same "missing endpoint" 404 signal handled by
    #' `do_generate_image()`, falls back to `POST /v1/responses` with the
    #' source image inlined as an `input_image` data URL and the optional
    #' mask passed via `input_image_mask` on the `image_generation` tool.
    #'
    #' The Responses fallback accepts a single source image per turn
    #' (multi-reference edit is classic-only). Image params (`quality`,
    #' `size`, `output_format`, `background`, `output_compression`,
    #' `moderation`, `input_fidelity`) are forwarded into the tool config,
    #' and `previous_response_id` is auto-attached from any prior fallback
    #' call so iterative edits chain.
    #'
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_edit_image = function(params) {
      if (is.null(params$image)) {
        rlang::abort("`image` must be supplied for OpenAI image editing.")
      }

      classic <- tryCatch(
        self$do_edit_image_classic(params),
        error = function(e) e
      )
      if (!inherits(classic, "error")) {
        return(classic)
      }

      if (self$looks_like_missing_classic_endpoint(classic)) {
        message(
          "OpenAI image edit: classic /v1/images/edits is unreachable on this endpoint. ",
          "Falling back to /v1/responses with the `image_generation` tool."
        )
        return(self$do_edit_image_via_responses(params))
      }

      stop(classic)
    },

    #' @description Edit images via the classic `POST /v1/images/edits`
    #'   multipart endpoint. Called by `do_edit_image()`; exposed for callers
    #'   that want to bypass the Responses-API fallback.
    #' @param params A list of call options (see `do_edit_image`).
    #' @return A GenerateImageResult object.
    do_edit_image_classic = function(params) {
      url <- api_endpoint_urls(private$config, "/images/edits")
      headers <- private$get_headers(include_content_type = FALSE)
      body <- private$build_edit_body(params)
      response <- post_multipart_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      GenerateImageResult$new(
        images = private$parse_image_response(
          response,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_edit",
          requested_output_format = body$output_format %||% NULL
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Stream image generation with partial-image previews via
    #'   `POST /v1/responses`. Sets `stream = TRUE` and `partial_images` on
    #'   the `image_generation` tool config; dispatches SSE events to the
    #'   user-supplied `callback` (one per partial frame, one final). Uses
    #'   the same Responses-API path as the non-streaming fallback, with the
    #'   same `previous_response_id` chaining.
    #' @param params A list of call options. The `partial_images` field
    #'   (0–3) controls how many preview frames the API emits before the
    #'   final image; default `2`.
    #' @param callback A function receiving each event list.
    #' @return A GenerateImageResult with the final image.
    do_stream_image = function(params, callback) {
      if (is.null(params$prompt) || !nzchar(params$prompt)) {
        rlang::abort("`prompt` must be a non-empty string.")
      }

      url <- api_endpoint_urls(private$config, "/responses")
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )

      tool_cfg <- private$build_responses_tool_config(params)
      partial_n <- as.integer(params$partial_images %||% 2)
      if (is.na(partial_n) || partial_n < 0 || partial_n > 3) {
        rlang::abort("`partial_images` must be an integer in 0..3.")
      }
      if (partial_n > 0) {
        tool_cfg$partial_images <- partial_n
      }

      body <- list(
        model = self$model_id,
        input = params$prompt,
        stream = TRUE,
        tools = list(tool_cfg)
      )
      if (private$use_server_state() && !is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      state <- new.env(parent = emptyenv())
      state$partial_count <- 0L
      state$final_images <- list()
      state$response_id <- NULL
      state$usage <- NULL

      stream_responses_api(
        url = url,
        headers = headers,
        body = body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds,
        callback = function(event_type, data, done) {
          if (isTRUE(done)) return(invisible(NULL))
          if (is.null(event_type) || is.null(data)) return(invisible(NULL))

          # Pick up the response id from response.created (or the first event
          # that carries it) so multi-turn chaining works even if completion
          # arrives via a different event shape.
          if (is.null(state$response_id)) {
            state$response_id <- data$response$id %||% data$id %||% NULL
          }

          # Partial previews. The documented event type is
          # `response.image_generation_call.partial_image` with a
          # `partial_image_b64` field; accept a few aliases for proxy
          # quirks.
          if (grepl("partial_image", event_type, fixed = TRUE)) {
            b64 <- data$partial_image_b64 %||% data$b64 %||% data$result
            if (!is.null(b64) && nzchar(b64)) {
              bytes <- base64enc::base64decode(b64)
              state$partial_count <- state$partial_count + 1L
              idx <- as.integer(data$partial_image_index %||% state$partial_count)
              callback(list(
                type = "partial",
                index = idx,
                bytes = bytes,
                media_type = media_type,
                done = FALSE
              ))
            }
            return(invisible(NULL))
          }

          # Per-call completion event.
          if (event_type == "response.image_generation_call.completed" &&
              !is.null(data$result)) {
            state$final_images <- c(state$final_images, list(list(
              bytes = base64enc::base64decode(data$result),
              media_type = media_type,
              revised_prompt = data$revised_prompt %||% NULL
            )))
            return(invisible(NULL))
          }

          # Final response envelope — has usage and any image calls we
          # haven't already collected via the per-call completion event.
          if (event_type == "response.completed" && is.list(data$response)) {
            if (is.null(state$response_id)) state$response_id <- data$response$id
            state$usage <- data$response$usage %||% NULL
            if (!length(state$final_images)) {
              for (item in data$response$output %||% list()) {
                if (identical(item$type %||% "", "image_generation_call") &&
                    !is.null(item$result)) {
                  state$final_images <- c(state$final_images, list(list(
                    bytes = base64enc::base64decode(item$result),
                    media_type = media_type,
                    revised_prompt = item$revised_prompt %||% NULL
                  )))
                }
              }
            }
          }

          invisible(NULL)
        }
      )

      if (!is.null(state$response_id)) {
        private$last_response_id <- state$response_id
      }

      if (!length(state$final_images)) {
        rlang::abort(c(
          "Streaming completed but no final image was received.",
          i = "The API may have sent partials only, or the connection ended before the final image_generation_call event."
        ))
      }

      callback(list(
        type = "completed",
        bytes = state$final_images[[1]]$bytes,
        media_type = media_type,
        done = TRUE
      ))

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          state$final_images,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image_stream"
        ),
        usage = state$usage,
        raw_response = list(response_id = state$response_id, partial_count = state$partial_count)
      )
    },

    #' @description Edit images via `POST /v1/responses` with the
    #'   `image_generation` tool in edit mode. Inlines the source image as a
    #'   base64 data URL inside an `input_image` block; passes the optional
    #'   mask via the tool's `input_image_mask` field.
    #' @param params A list of call options (see `do_edit_image`).
    #' @return A GenerateImageResult object.
    do_edit_image_via_responses = function(params) {
      url <- api_endpoint_urls(private$config, "/responses")
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )

      upload_dir <- params$output_dir %||% tempdir()
      image_inputs <- coerce_image_inputs(params$image)
      if (length(image_inputs) > 1) {
        rlang::warn(
          "Responses-API image edit accepts a single source image per turn; using only the first. Use the classic endpoint for multi-reference edits."
        )
      }
      src_data_url <- normalize_image_input_to_url_like(image_inputs[[1]])
      if (!grepl("^(data:|https?:)", src_data_url)) {
        # normalize_image_input_to_url_like returns either a URL/data-URI or
        # a bare base64 string — but for the Responses input_image block we
        # always want a data URI. Wrap the bare path case.
        src_data_url <- paste0("data:image/png;base64,", base64enc::base64encode(src_data_url))
      }

      mask_data_url <- NULL
      if (!is.null(params$mask)) {
        mask_inputs <- coerce_image_inputs(params$mask, arg = "`mask`")
        mask_data_url <- normalize_image_input_to_url_like(mask_inputs[[1]])
        if (!grepl("^(data:|https?:)", mask_data_url)) {
          mask_data_url <- paste0("data:image/png;base64,", base64enc::base64encode(mask_data_url))
        }
      }

      body <- list(
        model = self$model_id,
        input = list(list(
          role = "user",
          content = list(
            list(type = "input_text", text = params$prompt %||% "Edit this image."),
            list(type = "input_image", image_url = src_data_url)
          )
        )),
        tools = list(private$build_responses_edit_tool_config(params, mask_data_url))
      )
      if (private$use_server_state() && !is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      if (!is.null(response$id)) {
        private$last_response_id <- response$id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      images <- list()
      for (item in response$output %||% list()) {
        if (identical(item$type %||% "", "image_generation_call") && !is.null(item$result)) {
          images <- c(images, list(list(
            bytes = base64enc::base64decode(item$result),
            media_type = media_type,
            revised_prompt = item$revised_prompt %||% NULL
          )))
        }
      }

      if (!length(images)) {
        rlang::abort(c(
          "Responses API returned no `image_generation_call` output for the edit request.",
          i = "The proxy accepted the request but produced no edited image; this is usually a model or prompt issue."
        ))
      }

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          images,
          output_dir = upload_dir,
          prefix = "openai_edit_responses"
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    }
  )
)
