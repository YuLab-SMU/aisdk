# Layer 3: Gemini explicit context caching (CachedContent lifecycle + wiring).

fake_cache_response <- function() {
  list(name = "cachedContents/abc123", model = "models/gemini-2.5-flash",
       displayName = "my-cache", createTime = "2026-07-15T09:00:00Z",
       expireTime = "2026-07-15T10:00:00Z",
       usageMetadata = list(totalTokenCount = 5000))
}

test_that("gemini_create_cache POSTs to cachedContents and returns a handle", {
  cap <- new.env()
  local_mocked_bindings(
    request_json_from_api = function(url, headers, method = "GET", body = NULL, ...) {
      cap$url <- url; cap$method <- method; cap$body <- body
      fake_cache_response()
    },
    .package = "aisdk"
  )
  m <- create_gemini(api_key = "FAKE")$language_model("gemini-2.5-flash")
  cache <- gemini_create_cache(m, system = "a big stable system prompt", ttl = "600s",
                               display_name = "my-cache")

  expect_s3_class(cache, "aisdk_gemini_cache")
  expect_equal(cache$name, "cachedContents/abc123")
  expect_equal(cache$token_count, 5000)
  expect_equal(cap$method, "POST")
  expect_true(grepl("/cachedContents?key=", cap$url, fixed = TRUE))
  expect_equal(cap$body$model, "models/gemini-2.5-flash")
  expect_equal(cap$body$ttl, "600s")
  expect_false(is.null(cap$body$systemInstruction))
})

test_that("cache lifecycle ops use the right HTTP method and URL", {
  cap <- new.env()
  local_mocked_bindings(
    request_json_from_api = function(url, headers, method = "GET", body = NULL, ...) {
      cap$url <- url; cap$method <- method; cap$body <- body
      if (identical(method, "GET") && grepl("cachedContents?", url, fixed = TRUE) &&
          !grepl("abc123", url)) {
        return(list(cachedContents = list(fake_cache_response())))
      }
      fake_cache_response()
    },
    .package = "aisdk"
  )
  m <- create_gemini(api_key = "FAKE")$language_model("gemini-2.5-flash")

  gemini_delete_cache(m, "cachedContents/abc123")
  expect_equal(cap$method, "DELETE")
  expect_true(grepl("cachedContents/abc123", cap$url, fixed = TRUE))

  gemini_get_cache(m, "cachedContents/abc123")
  expect_equal(cap$method, "GET")
  expect_true(grepl("cachedContents/abc123", cap$url, fixed = TRUE))

  gemini_update_cache(m, "cachedContents/abc123", ttl = "120s")
  expect_equal(cap$method, "PATCH")
  expect_true(grepl("updateMask=ttl", cap$url, fixed = TRUE))
  expect_equal(cap$body$ttl, "120s")

  caches <- gemini_list_caches(m)
  expect_equal(cap$method, "GET")
  expect_length(caches, 1)
  expect_s3_class(caches[[1]], "aisdk_gemini_cache")
})

test_that("referencing a cache sets cachedContent and strips system/tools/toolConfig", {
  m <- create_gemini(api_key = "FAKE")$language_model("gemini-2.5-flash")
  a_tool <- tool(name = "t", description = "d",
                 parameters = z_object(x = z_string("x")), execute = function(x) x)
  cache <- structure(list(name = "cachedContents/abc123"), class = "aisdk_gemini_cache")

  body <- m$build_payload_internal(list(
    messages = list(list(role = "system", content = "SYS"), list(role = "user", content = "hi")),
    tools = list(a_tool), tool_choice = "required", cached_content = cache
  ), stream = FALSE)$body

  expect_equal(body$cachedContent, "cachedContents/abc123")
  expect_null(body$systemInstruction) # must not co-exist with a cache
  expect_null(body$tools)
  expect_null(body$toolConfig)
  expect_null(body$cached_content)     # the param never becomes a wire field

  # A raw name string works too.
  body2 <- m$build_payload_internal(
    list(messages = list(list(role = "user", content = "hi")), cached_content = "cachedContents/xyz"),
    stream = FALSE)$body
  expect_equal(body2$cachedContent, "cachedContents/xyz")
})

test_that("cached_content never leaks into non-Gemini provider bodies", {
  p <- list(messages = list(list(role = "user", content = "hi")), cached_content = "cachedContents/x")
  oc <- create_openai(api_key = "FAKE")$language_model("gpt-4o")$.__enclos_env__$private
  expect_null(oc$build_chat_body(p, stream = FALSE)$cached_content)
  orr <- create_openai(api_key = "FAKE")$responses_model("gpt-4o")$.__enclos_env__$private
  expect_null(orr$build_responses_body(p, stream = FALSE)$cached_content)
  an <- create_anthropic(api_key = "FAKE")$language_model("claude-sonnet-5")$.__enclos_env__$private
  expect_null(an$build_messages_body(p, stream = FALSE)$cached_content)
})

test_that("cache helpers and non-Gemini rejection behave", {
  expect_equal(aisdk:::gemini_cache_name("cachedContents/x"), "cachedContents/x")
  expect_equal(aisdk:::gemini_cache_name(structure(list(name = "cachedContents/y"),
                                                   class = "aisdk_gemini_cache")), "cachedContents/y")
  expect_null(aisdk:::gemini_cache_name(NULL))

  h <- aisdk:::gemini_cache_from_response(fake_cache_response(), "gemini-2.5-flash")
  expect_s3_class(h, "aisdk_gemini_cache")
  expect_equal(h$token_count, 5000)
  expect_null(aisdk:::gemini_cache_from_response(list(), "x")) # no name -> NULL
  expect_output(print(h), "Gemini context cache")

  expect_error(gemini_create_cache(create_openai(api_key = "FAKE")$language_model("gpt-4o")),
               "requires a Gemini model")
})
