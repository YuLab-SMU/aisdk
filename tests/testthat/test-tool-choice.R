# Portable tool_choice: one unified value maps to each provider's native shape.
# See normalize_tool_choice() in R/content_translation.R and the per-provider
# body builders. Without this, tool-control code written for one provider
# silently breaks on the others (Gemini dropped tool_choice entirely; Anthropic
# 400ed on a passed-through parallel_tool_calls).

test_that("normalize_tool_choice maps the unified modes for OpenAI chat", {
  n <- aisdk:::normalize_tool_choice
  expect_equal(n("auto", "openai_chat"), "auto")
  expect_equal(n("none", "openai_chat"), "none")
  expect_equal(n("required", "openai_chat"), "required")
  expect_equal(n("any", "openai_chat"), "required")            # alias
  expect_equal(n("REQUIRED", "openai_chat"), "required")        # case-insensitive
  expect_equal(
    n(list(type = "tool", name = "get_weather"), "openai_chat"),
    list(type = "function", `function` = list(name = "get_weather"))
  )
  expect_null(n(NULL, "openai_chat"))
})

test_that("normalize_tool_choice uses the flat Responses tool shape", {
  n <- aisdk:::normalize_tool_choice
  expect_equal(
    n(list(type = "tool", name = "get_weather"), "openai_responses"),
    list(type = "function", name = "get_weather")               # no nested `function`
  )
  expect_equal(n("required", "openai_responses"), "required")
})

test_that("normalize_tool_choice maps to Anthropic's object shape + disable_parallel", {
  n <- aisdk:::normalize_tool_choice
  expect_equal(n("auto", "anthropic"), list(type = "auto"))
  expect_equal(n("required", "anthropic"), list(type = "any"))
  expect_equal(n("any", "anthropic"), list(type = "any"))
  expect_equal(n("none", "anthropic"), list(type = "none"))
  expect_equal(n(list(type = "tool", name = "x"), "anthropic"), list(type = "tool", name = "x"))
  # parallel = FALSE rides inside tool_choice on Anthropic...
  expect_equal(
    n("required", "anthropic", parallel = FALSE),
    list(type = "any", disable_parallel_tool_use = TRUE)
  )
  # ...and defaults to an auto carrier when no explicit choice was given.
  expect_equal(
    n(NULL, "anthropic", parallel = FALSE),
    list(type = "auto", disable_parallel_tool_use = TRUE)
  )
  # "none" never carries disable_parallel (there are no calls to disable).
  expect_equal(n("none", "anthropic", parallel = FALSE), list(type = "none"))
})

test_that("normalize_tool_choice maps to Gemini functionCallingConfig", {
  n <- aisdk:::normalize_tool_choice
  expect_equal(n("auto", "gemini"), list(functionCallingConfig = list(mode = "AUTO")))
  expect_equal(n("required", "gemini"), list(functionCallingConfig = list(mode = "ANY")))
  expect_equal(n("none", "gemini"), list(functionCallingConfig = list(mode = "NONE")))
  expect_equal(
    n(list(type = "tool", name = "get_weather"), "gemini"),
    list(functionCallingConfig = list(mode = "ANY", allowedFunctionNames = list("get_weather")))
  )
})

test_that("normalize_tool_choice passes provider-native values through unchanged", {
  n <- aisdk:::normalize_tool_choice
  native_oa <- list(type = "function", `function` = list(name = "x"))
  expect_equal(n(native_oa, "openai_chat"), native_oa)          # already OpenAI-shaped
  native_an <- list(type = "any")
  expect_equal(n(native_an, "anthropic"), native_an)            # already Anthropic-shaped
})

test_that("tool_choice reaches each provider body in the right slot and doesn't leak", {
  skip_if_offline <- tryCatch(!curl::has_internet(), error = function(e) FALSE)
  msgs <- list(list(role = "user", content = "hi"))
  a_tool <- tool(name = "get_weather", description = "d",
                 parameters = z_object(x = z_string("x")), execute = function(x) x)
  tools <- list(a_tool)

  # OpenAI chat: unified name -> function object; parallel_tool_calls native.
  oc <- create_openai(api_key = "FAKE")$language_model("gpt-4o")$.__enclos_env__$private
  b <- oc$build_chat_body(list(messages = msgs, tools = tools,
                               tool_choice = list(type = "tool", name = "get_weather")), stream = FALSE)
  expect_equal(b$tool_choice, list(type = "function", `function` = list(name = "get_weather")))
  b <- oc$build_chat_body(list(messages = msgs, tools = tools, parallel_tool_calls = FALSE), stream = FALSE)
  expect_false(b$parallel_tool_calls)                          # OpenAI-native, preserved

  # Anthropic: parallel_tool_calls must NOT leak (would 400); it rides in tool_choice.
  an <- create_anthropic(api_key = "FAKE")$language_model("claude-sonnet-5")$.__enclos_env__$private
  b <- an$build_messages_body(list(messages = msgs, tools = tools,
                                   tool_choice = "required", parallel_tool_calls = FALSE), stream = FALSE)
  expect_equal(b$tool_choice, list(type = "any", disable_parallel_tool_use = TRUE))
  expect_null(b$parallel_tool_calls)

  # Gemini: unified value is the only path into the body (no generic passthrough).
  ge <- create_gemini(api_key = "FAKE")$language_model("gemini-2.0-flash")
  b <- ge$build_payload_internal(list(messages = msgs, tools = tools, tool_choice = "none"), stream = FALSE)$body
  expect_equal(b$toolConfig, list(functionCallingConfig = list(mode = "NONE")))
  expect_null(b$tool_choice)
})
