# Tests for Anthropic Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

# Get model names from environment
anthropic_model <- get_anthropic_model()
anthropic_base_url <- get_anthropic_base_url()

test_that("create_anthropic() creates a provider with correct defaults", {
  # Use safe provider creation
  provider <- safe_create_provider(create_anthropic)

  expect_s3_class(provider, "AnthropicProvider")
  expect_equal(provider$specification_version, "v1")
})

test_that("Anthropic provider creates language model correctly", {
  provider <- safe_create_provider(create_anthropic)
  model <- provider$language_model(anthropic_model)

  expect_s3_class(model, "AnthropicLanguageModel")
  expect_equal(model$model_id, anthropic_model)
  expect_equal(model$provider, "anthropic")
  expect_equal(model$specification_version, "v1")
})

test_that("Anthropic provider uses custom API version", {
  provider <- safe_create_provider(create_anthropic,
    api_version = "2024-01-01"
  )
  # Provider should be created successfully
  expect_s3_class(provider, "AnthropicProvider")
})

test_that("Anthropic provider stores multiple base URLs for failover", {
  provider <- suppressWarnings(create_anthropic(
    api_key = "test-key",
    base_url = "https://primary.example/v1, https://backup.example/v1"
  ))
  model <- provider$language_model(anthropic_model)
  config <- model$get_config()

  expect_equal(config$base_url, "https://primary.example/v1")
  expect_equal(config$base_urls, c("https://primary.example/v1", "https://backup.example/v1"))
})

test_that("create_anthropic() warns when API key is missing", {
  # Temporarily unset API key
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  expect_warning(
    create_anthropic(),
    "Anthropic API key not set"
  )
})

# Live API tests (only run when API key is available)
test_that("Anthropic provider can make real API calls", {
  skip_if_no_api_key("Anthropic")
  skip_on_cran()

  # Skip if we are running under an AiHubMix base url to avoid auth errors
  url <- Sys.getenv("ANTHROPIC_BASE_URL")
  if (nzchar(url) && grepl("aihubmix", url)) {
    skip("Skipping real API call because ANTHROPIC_BASE_URL is set to AiHubMix")
  }

  provider <- create_anthropic()
  model <- provider$language_model(anthropic_model)

  # Make a simple API call
  result <- model$generate(
    messages = list(
      list(role = "user", content = "Say 'Hello, World!'")
    ),
    max_tokens = 20
  )

  # Check that we got a response
  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)
})

test_that("Anthropic provider handles tool calls", {
  skip_if_no_api_key("Anthropic")
  skip_on_cran()

  url <- Sys.getenv("ANTHROPIC_BASE_URL")
  if (nzchar(url) && grepl("aihubmix", url)) {
    skip("Skipping real API call because ANTHROPIC_BASE_URL is set to AiHubMix")
  }

  provider <- create_anthropic()
  model <- provider$language_model(anthropic_model)

  # Create a simple test tool
  test_tool <- Tool$new(
    name = "get_time",
    description = "Get the current time",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      paste0("Current time: ", Sys.time())
    }
  )

  # Call model with tool
  result <- model$generate(
    messages = list(
      list(role = "user", content = "What time is it?")
    ),
    tools = list(test_tool),
    max_tokens = 50
  )

  # Check response (can be text or tool calls)
  expect_true(!is.null(result$text) || length(result$tool_calls) > 0)
})

test_that("Anthropic provider generates tool definition with cache_control", {
  skip_on_cran()

  provider <- create_anthropic(api_key = "test_key", base_url = "https://api.anthropic.com/v1")
  model <- provider$language_model(anthropic_model)

  # Create a simple test tool with caching enabled
  test_tool <- Tool$new(
    name = "get_weather",
    description = "Get the weather",
    parameters = z_object(location = z_string("city")),
    execute = function(args) {
      "sunny"
    },
    meta = list(cache_control = list(type = "ephemeral"))
  )

  tool_api <- test_tool$to_api_format("anthropic")

  if (!is.null(test_tool$meta) && !is.null(test_tool$meta$cache_control)) {
    tool_api$cache_control <- test_tool$meta$cache_control
  }

  expect_equal(tool_api$name, "get_weather")
  expect_equal(tool_api$cache_control$type, "ephemeral")
})

test_that("Anthropic formatter translates multimodal content blocks", {
  provider <- safe_create_provider(create_anthropic)
  model <- provider$language_model(anthropic_model)

  fake_response <- list(
    content = list(list(type = "text", text = "done")),
    usage = list(input_tokens = 1, output_tokens = 1),
    stop_reason = "end_turn"
  )
  captured <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured <<- body
      fake_response
    }
  )

  model$do_generate(list(
    messages = list(
      list(role = "system", content = list(input_text("You are helpful."))),
      list(
        role = "user",
        content = list(
          input_text("Describe this image"),
          input_image("https://example.com/test.png", media_type = "image/png")
        )
      )
    ),
    max_tokens = 20
  ))

  expect_equal(captured$system, "You are helpful.")
  expect_equal(captured$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured$messages[[1]]$content[[2]]$source$type, "url")
  expect_equal(captured$messages[[1]]$content[[2]]$source$url, "https://example.com/test.png")
})

test_that("Anthropic payload translates multimodal content blocks", {
  provider <- safe_create_provider(create_anthropic, api_key = "test_key")
  model <- provider$language_model(anthropic_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        content = list(list(type = "text", text = "ok")),
        stop_reason = "end_turn",
        usage = list(input_tokens = 10, output_tokens = 2)
      )
    }
  )

  result <- model$do_generate(list(
    messages = list(
      list(
        role = "user",
        content = list(
          input_text("Analyze this image"),
          input_image(
            paste0(
              "data:image/png;base64,",
              base64enc::base64encode(charToRaw("fake-image"))
            ),
            media_type = "image/png"
          )
        )
      )
    ),
    max_tokens = 128
  ))

  expect_equal(result$text, "ok")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Analyze this image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$type, "base64")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$media_type, "image/png")
})

test_that("Anthropic payload translates provider-neutral multimodal blocks", {
  provider <- create_anthropic(api_key = "test_key", base_url = "https://api.anthropic.com/v1")
  model <- provider$language_model(anthropic_model)

  image_path <- tempfile(fileext = ".png")
  writeBin(as.raw(0:15), image_path)
  on.exit(unlink(image_path), add = TRUE)

  captured_body <- NULL
  mock_response <- list(
    content = list(list(type = "text", text = "ok")),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      mock_response
    },
    .package = "aisdk"
  )

  result <- model$generate(
    messages = list(list(
      role = "user",
      content = list(
        input_text("Analyze the image."),
        input_image(image_path)
      )
    )),
    max_tokens = 32
  )

  expect_equal(result$text, "ok")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$type, "base64")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$media_type, "image/png")
})

# --- W3: shared Messages-API builder, stream/non-stream parity ---------------

test_that("Anthropic stream and non-stream bodies agree modulo stream field", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-4-20250514")
  params <- list(
    messages = list(list(role = "user", content = "hi")),
    temperature = 0.3,
    top_p = 0.9,
    max_tokens = 128,
    stop_sequences = c("END"),
    seed = 42,
    timeout_seconds = 5
  )

  gen <- model$.__enclos_env__$private$build_messages_body(params, stream = FALSE)
  strm <- model$.__enclos_env__$private$build_messages_body(params, stream = TRUE)

  expect_null(gen$stream)
  expect_true(isTRUE(strm$stream))
  drop_stream <- function(b) b[setdiff(names(b), "stream")]
  expect_equal(drop_stream(gen), drop_stream(strm))

  # Fields the stream path used to silently drop are now present.
  expect_equal(strm$top_p, 0.9)
  expect_equal(strm$stop_sequences, "END")
  expect_equal(strm$seed, 42) # extra-params passthrough
  expect_null(strm$timeout_seconds) # per-call timeouts stay out of the body
})

test_that("Anthropic thinking forces temperature 1.0 and passes through in both paths", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-4-20250514")
  params <- list(
    messages = list(list(role = "user", content = "hi")),
    temperature = 0.3,
    thinking = list(type = "enabled", budget_tokens = 2048)
  )

  for (stream in c(FALSE, TRUE)) {
    body <- model$.__enclos_env__$private$build_messages_body(params, stream = stream)
    expect_equal(body$temperature, 1.0)
    expect_equal(body$thinking$budget_tokens, 2048)
  }
})

test_that("reasoning_effort maps to an Anthropic thinking budget (portable reasoning)", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-4-20250514")
  priv <- model$.__enclos_env__$private

  expected <- list(low = 2048L, medium = 8192L, high = 16384L)
  for (effort in names(expected)) {
    params <- list(messages = list(list(role = "user", content = "hi")),
                   reasoning_effort = effort)
    for (stream in c(FALSE, TRUE)) {
      body <- priv$build_messages_body(params, stream = stream)
      expect_equal(body$thinking$type, "enabled")
      expect_equal(body$thinking$budget_tokens, expected[[effort]])
      expect_equal(body$temperature, 1.0)      # thinking forces temperature 1
      expect_null(body$reasoning_effort)         # raw param never leaks into the body
    }
  }
})

test_that("reasoning_effort defers to an explicit thinking block and ignores junk values", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-4-20250514")
  priv <- model$.__enclos_env__$private
  msgs <- list(list(role = "user", content = "hi"))

  # An explicit thinking block wins over reasoning_effort.
  won <- priv$build_messages_body(list(
    messages = msgs, reasoning_effort = "high",
    thinking = list(type = "enabled", budget_tokens = 999)
  ), stream = FALSE)
  expect_equal(won$thinking$budget_tokens, 999)

  # An unrecognized effort adds no thinking block (and still doesn't leak).
  none <- priv$build_messages_body(list(messages = msgs, reasoning_effort = "turbo"),
                                   stream = FALSE)
  expect_null(none$thinking)
  expect_null(none$reasoning_effort)

  # Overridable via options().
  withr::with_options(
    list(aisdk.anthropic_reasoning_budgets = list(low = 100L, medium = 200L, high = 300L)),
    {
      b <- priv$build_messages_body(list(messages = msgs, reasoning_effort = "medium"),
                                    stream = FALSE)
      expect_equal(b$thinking$budget_tokens, 200L)
    }
  )
})

test_that("Anthropic streaming usage reports prompt tokens from message_start", {
  agg <- SSEAggregator$new(function(text, done) invisible())
  aisdk:::map_anthropic_chunk(
    "message_start", list(message = list(usage = list(input_tokens = 850))), agg
  )
  aisdk:::map_anthropic_chunk(
    "content_block_delta", list(delta = list(type = "text_delta", text = "hi")), agg
  )
  aisdk:::map_anthropic_chunk(
    "message_delta",
    list(delta = list(stop_reason = "end_turn"), usage = list(output_tokens = 200)), agg
  )
  res <- agg$build_result()
  expect_equal(res$usage$prompt_tokens, 850)
  expect_equal(res$usage$completion_tokens, 200)
  expect_equal(res$usage$total_tokens, 1050)
})

test_that("agent runtime rebuilds Anthropic tool_use blocks from a streamed result", {
  model <- safe_create_provider(create_anthropic, api_key = "FAKE")$language_model("claude-sonnet-4")
  tcs <- list(list(id = "c1", name = "get_weather", arguments = list(city = "SF")))
  # Stream result: raw_response is the last SSE event, not a content array.
  res <- list(text = "", tool_calls = tcs, raw_response = list(type = "message_stop"))
  msgs <- aisdk:::agent_runtime_append_provider_messages(list(), model, res, list())$messages
  assistant <- msgs[[1]]
  expect_equal(assistant$content[[1]]$type, "tool_use")
  expect_equal(assistant$content[[1]]$id, "c1")
  expect_equal(assistant$content[[1]]$name, "get_weather")
})

# --- AD1: streamed extended-thinking signature is replayed before tool_use ----
# Anthropic rejects the next turn unless the signed thinking block that preceded
# a tool_use is passed back ("a final assistant message must start with a
# thinking block"). Non-streaming keeps it via raw_response$content; the stream
# drops content on message_stop, so it is rebuilt from the captured signature.

test_that("streamed Anthropic thinking block + signature leads the tool_use turn", {
  model <- safe_create_provider(create_anthropic, api_key = "FAKE")$language_model("claude-sonnet-4")
  tcs <- list(list(id = "c1", name = "get_weather", arguments = list(city = "SF")))
  res <- list(text = "", tool_calls = tcs, reasoning = "Let me check the weather.",
              reasoning_signature = "SIGxyz", raw_response = list(type = "message_stop"))
  content <- aisdk:::agent_runtime_append_provider_messages(list(), model, res, list())$messages[[1]]$content
  expect_equal(content[[1]]$type, "thinking")            # must START with thinking
  expect_equal(content[[1]]$thinking, "Let me check the weather.")
  expect_equal(content[[1]]$signature, "SIGxyz")
  expect_equal(content[[2]]$type, "tool_use")            # then the tool_use
  expect_equal(content[[2]]$id, "c1")
})

test_that("streamed thinking without a signature is omitted (an unsigned block 400s)", {
  model <- safe_create_provider(create_anthropic, api_key = "FAKE")$language_model("claude-sonnet-4")
  tcs <- list(list(id = "c1", name = "get_weather", arguments = list(city = "SF")))
  res <- list(text = "", tool_calls = tcs, reasoning = "unsigned thoughts",
              reasoning_signature = NULL, raw_response = list(type = "message_stop"))
  content <- aisdk:::agent_runtime_append_provider_messages(list(), model, res, list())$messages[[1]]$content
  expect_equal(content[[1]]$type, "tool_use")            # no invalid thinking block
})

test_that("streamed redacted_thinking is replayed before tool_use", {
  model <- safe_create_provider(create_anthropic, api_key = "FAKE")$language_model("claude-sonnet-4")
  tcs <- list(list(id = "c1", name = "get_weather", arguments = list(city = "SF")))
  res <- list(text = "", tool_calls = tcs,
              redacted_thinking = list(list(type = "redacted_thinking", data = "BLOB")),
              raw_response = list(type = "message_stop"))
  content <- aisdk:::agent_runtime_append_provider_messages(list(), model, res, list())$messages[[1]]$content
  expect_equal(content[[1]]$type, "redacted_thinking")
  expect_equal(content[[1]]$data, "BLOB")
  expect_equal(content[[2]]$type, "tool_use")
})

# --- U3: server-side context editing (tool-result clearing) passthrough -------

test_that("Anthropic per-call context_management reaches the body and sets the beta", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-4-20250514")
  priv <- model$.__enclos_env__$private
  cm <- list(edits = list(list(
    type = "clear_tool_uses_20250919",
    trigger = list(type = "input_tokens", value = 30000),
    keep = list(type = "tool_uses", value = 3)
  )))

  body <- priv$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), context_management = cm),
    stream = FALSE
  )
  expect_equal(body$context_management$edits[[1]]$type, "clear_tool_uses_20250919")

  # Beta header only when context management is active.
  expect_match(priv$get_headers(context_management = TRUE)$`anthropic-beta`,
               "context-management-2025-06-27")
  expect_null(priv$get_headers(context_management = FALSE)$`anthropic-beta`)
})

test_that("Anthropic caching and context-editing betas coexist in one header", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  provider$enable_caching(TRUE)
  priv <- provider$language_model("claude-sonnet-4")$.__enclos_env__$private
  beta <- priv$get_headers(context_management = TRUE)$`anthropic-beta`
  expect_match(beta, "prompt-caching-2024-07-31")
  expect_match(beta, "context-management-2025-06-27")
  # caching alone stays single-valued
  expect_equal(priv$get_headers(context_management = FALSE)$`anthropic-beta`,
               "prompt-caching-2024-07-31")
})

# --- V1: enable_caching auto-places a breakpoint and surfaces cache tokens ----

test_that("enable_caching auto-marks the last tool so the tools prefix caches", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  provider$enable_caching(TRUE)
  priv <- provider$language_model("claude-sonnet-4")$.__enclos_env__$private
  t1 <- tool(name = "a", description = "tool a", execute = function() "x")
  t2 <- tool(name = "b", description = "tool b", execute = function() "y")

  body <- priv$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), tools = list(t1, t2)),
    stream = FALSE
  )
  n <- length(body$tools)
  expect_equal(body$tools[[n]]$cache_control$type, "ephemeral")
  expect_null(body$tools[[1]]$cache_control) # only the last tool marked
})

test_that("caching disabled places no auto breakpoint; caller mark is respected", {
  # Disabled: no breakpoint.
  off <- safe_create_provider(create_anthropic, api_key = "FAKE")
  t1 <- tool(name = "a", description = "a", execute = function() "x")
  b_off <- off$language_model("claude-sonnet-4")$.__enclos_env__$private$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), tools = list(t1)), stream = FALSE
  )
  expect_null(b_off$tools[[1]]$cache_control)

  # Enabled but the caller already marked a tool: don't add a second breakpoint.
  on <- safe_create_provider(create_anthropic, api_key = "FAKE")
  on$enable_caching(TRUE)
  t_marked <- tool(name = "a", description = "a", execute = function() "x",
                   meta = list(cache_control = list(type = "ephemeral")))
  t_plain <- tool(name = "b", description = "b", execute = function() "y")
  b_on <- on$language_model("claude-sonnet-4")$.__enclos_env__$private$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), tools = list(t_marked, t_plain)),
    stream = FALSE
  )
  expect_equal(b_on$tools[[1]]$cache_control$type, "ephemeral") # caller's mark
  expect_null(b_on$tools[[2]]$cache_control) # not auto-added elsewhere
})

test_that("Anthropic usage surfaces cache tokens for observability", {
  u <- aisdk:::anthropic_usage_with_cache(list(
    input_tokens = 100, output_tokens = 20,
    cache_read_input_tokens = 800, cache_creation_input_tokens = 50
  ))
  expect_equal(u$cache_read_input_tokens, 800)
  expect_equal(u$cache_creation_input_tokens, 50)
  expect_equal(u$total_tokens, 970)

  # Without caching, cache fields are absent (not zero-noise).
  u2 <- aisdk:::anthropic_usage_with_cache(list(input_tokens = 100, output_tokens = 20))
  expect_null(u2$cache_read_input_tokens)
  expect_equal(u2$total_tokens, 120)
})

# --- X1: Anthropic structured outputs (z_schema response_format) --------------

test_that("Anthropic converts a z_schema response_format to output_config.format", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  priv <- provider$language_model("claude-sonnet-5")$.__enclos_env__$private
  schema <- z_object(name = z_string("the name"), age = z_number("the age"))

  body <- priv$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), response_format = schema),
    stream = FALSE
  )
  expect_equal(body$output_config$format$type, "json_schema")
  expect_false(is.null(body$output_config$format$schema$properties$name))
  # response_format must not leak into the body verbatim (used to 400).
  expect_null(body$response_format)
})

test_that("Anthropic structured-output fallback injects the schema into the system prompt", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  model <- provider$language_model("claude-sonnet-5")
  model$.__enclos_env__$private$config$disable_json_schema <- TRUE
  schema <- z_object(city = z_string("city"))

  body <- model$.__enclos_env__$private$build_messages_body(
    list(
      messages = list(
        list(role = "system", content = "You help."),
        list(role = "user", content = "hi")
      ),
      response_format = schema
    ),
    stream = FALSE
  )
  expect_null(body$output_config)
  expect_match(body$system, "adhering to this schema")
  expect_match(body$system, "You help.")
})

test_that("Anthropic leaves the body untouched when no response_format is given", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  priv <- provider$language_model("claude-sonnet-5")$.__enclos_env__$private
  body <- priv$build_messages_body(
    list(messages = list(list(role = "user", content = "hi"))), stream = FALSE
  )
  expect_null(body$output_config)
})

# --- Layer 1: system prefix caching (stable base cached, volatile tail not) --
# The runtime fuses a stable base system prompt + volatile per-turn context into
# one string; caching the whole thing never hits. When caching is on and a
# stable prefix is flagged (system_cache_prefix), only the prefix is cached.

test_that("Anthropic splits the system into a cached prefix + uncached volatile tail", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  provider$enable_caching(TRUE)
  priv <- provider$language_model("claude-sonnet-4")$.__enclos_env__$private

  stable <- paste(rep("STABLE BASE PROMPT. ", 120), collapse = "") # > 1024 chars
  volatile <- "VOLATILE PER-TURN CONTEXT"
  full <- paste(stable, volatile, sep = "\n\n")

  for (stream in c(FALSE, TRUE)) {
    sys <- priv$build_messages_body(
      list(messages = list(list(role = "system", content = full),
                           list(role = "user", content = "hi")),
           system_cache_prefix = stable),
      stream = stream
    )$system
    expect_true(is.list(sys) && length(sys) == 2)
    expect_equal(sys[[1]]$text, stable)
    expect_equal(sys[[1]]$cache_control, list(type = "ephemeral"))
    expect_true(grepl("VOLATILE", sys[[2]]$text))
    expect_null(sys[[2]]$cache_control)   # the volatile tail stays outside the cache
  }
})

test_that("system prefix caching is a no-op without caching, prefix, match, or length", {
  stable <- paste(rep("STABLE BASE PROMPT. ", 120), collapse = "")
  full <- paste(stable, "VOLATILE", sep = "\n\n")
  msgs <- list(list(role = "system", content = full), list(role = "user", content = "hi"))
  sys_of <- function(provider, params) {
    provider$language_model("claude-sonnet-4")$.__enclos_env__$private$build_messages_body(params, FALSE)$system
  }

  # caching OFF -> plain string
  off <- safe_create_provider(create_anthropic, api_key = "FAKE")
  expect_type(sys_of(off, list(messages = msgs, system_cache_prefix = stable)), "character")

  on <- safe_create_provider(create_anthropic, api_key = "FAKE"); on$enable_caching(TRUE)
  # no prefix flagged
  expect_type(sys_of(on, list(messages = msgs)), "character")
  # prefix is not actually a prefix of the system
  wrong <- paste(rep("WRONG PREFIX PAD. ", 120), collapse = "")
  expect_type(sys_of(on, list(messages = msgs, system_cache_prefix = wrong)), "character")
  # prefix below the min-chars threshold
  short <- list(list(role = "system", content = "short\n\nvol"), list(role = "user", content = "hi"))
  expect_type(sys_of(on, list(messages = short, system_cache_prefix = "short")), "character")
})

test_that("a whole stable system (no volatile tail) becomes one cached block; no leak", {
  provider <- safe_create_provider(create_anthropic, api_key = "FAKE")
  provider$enable_caching(TRUE)
  priv <- provider$language_model("claude-sonnet-4")$.__enclos_env__$private
  stable <- paste(rep("STABLE BASE PROMPT. ", 120), collapse = "")

  sys <- priv$build_messages_body(
    list(messages = list(list(role = "system", content = stable),
                         list(role = "user", content = "hi")),
         system_cache_prefix = stable), stream = FALSE)$system
  expect_true(is.list(sys) && length(sys) == 1)
  expect_equal(sys[[1]]$cache_control, list(type = "ephemeral"))

  # system_cache_prefix is consumed, never sent on the wire.
  body <- priv$build_messages_body(
    list(messages = list(list(role = "user", content = "hi")), system_cache_prefix = "x"),
    stream = FALSE)
  expect_null(body$system_cache_prefix)
})
