# AF1: create_embeddings passes provider options (dimensions, etc.) through to
# the request body. Previously do_embed(value) sent only {model, input}, so
# there was no way to request Matryoshka dimension truncation or other params.
#
# The mock must be installed inside each test_that (local_mocked_bindings scopes
# to its calling frame), so it is repeated rather than hoisted into a helper.

mock_embedding_post <- function(captured) {
  function(url, headers, body, ...) {
    captured$body <- body
    list(data = list(list(embedding = list(0.1, 0.2))), usage = list(prompt_tokens = 3))
  }
}

test_that("create_embeddings sends only model + input when no options are given", {
  cap <- new.env()
  local_mocked_bindings(post_to_api = mock_embedding_post(cap), .package = "aisdk")
  m <- create_openai(api_key = "FAKE")$embedding_model("text-embedding-3-small")
  res <- create_embeddings(m, "hello")
  expect_setequal(names(cap$body), c("model", "input"))
  expect_equal(res$embeddings[[1]], list(0.1, 0.2))
  expect_equal(res$usage$prompt_tokens, 3)
})

test_that("create_embeddings forwards dimensions as an integer", {
  cap <- new.env()
  local_mocked_bindings(post_to_api = mock_embedding_post(cap), .package = "aisdk")
  m <- create_openai(api_key = "FAKE")$embedding_model("text-embedding-3-small")
  create_embeddings(m, "hello", dimensions = 256)
  expect_identical(cap$body$dimensions, 256L)   # integer, not 256.0 (endpoint rejects the float)
})

test_that("create_embeddings batches a vector input in one request and forwards other opts", {
  cap <- new.env()
  local_mocked_bindings(post_to_api = mock_embedding_post(cap), .package = "aisdk")
  m <- create_openai(api_key = "FAKE")$embedding_model("text-embedding-3-small")
  create_embeddings(m, c("a", "b", "c"), dimensions = 512, user = "u1", encoding_format = "float")
  expect_length(cap$body$input, 3)              # one batched request, not three calls
  expect_identical(cap$body$dimensions, 512L)
  expect_equal(cap$body$user, "u1")
  expect_equal(cap$body$encoding_format, "float")
})

test_that("a NULL option does not leak an empty field into the body", {
  cap <- new.env()
  local_mocked_bindings(post_to_api = mock_embedding_post(cap), .package = "aisdk")
  m <- create_openai(api_key = "FAKE")$embedding_model("text-embedding-3-small")
  create_embeddings(m, "hello", dimensions = NULL)
  expect_false("dimensions" %in% names(cap$body))
})

test_that("registry is still accepted by name after the ... signature change", {
  cap <- new.env()
  local_mocked_bindings(post_to_api = mock_embedding_post(cap), .package = "aisdk")
  m <- create_openai(api_key = "FAKE")$embedding_model("text-embedding-3-small")
  expect_no_error(create_embeddings(m, "hello", registry = NULL))
  expect_setequal(names(cap$body), c("model", "input"))
})
