# Tests for semantic_search (Z6): embedding-based document ranking

# A deterministic mock embedding model: keyword -> fixed orthogonal vector, so
# cosine ranking is predictable without a network call.
MockEmbed <- R6::R6Class("MockEmbed",
  inherit = EmbeddingModelV1,
  public = list(
    initialize = function() super$initialize(provider = "mock", model_id = "mock-embed"),
    do_embed = function(value) {
      vecs <- lapply(value, function(t) {
        if (grepl("cat", t)) c(1, 0, 0)
        else if (grepl("dog", t)) c(0, 1, 0)
        else c(0, 0, 1)
      })
      list(embeddings = vecs, usage = list(total_tokens = length(value)))
    }
  )
)

test_that("semantic_search ranks documents by cosine similarity to the query", {
  docs <- c("a fluffy cat sits", "a loyal dog barks", "the weather is nice", "cats and kittens")
  res <- semantic_search("feline pet cat", docs, model = MockEmbed$new(), top_k = 2)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true(grepl("cat", res$document[1])) # closest to the cat query
  expect_true(all(diff(res$score) <= 0))     # descending score
  expect_true(all(res$index %in% seq_along(docs)))
})

test_that("semantic_search returns empty for no documents and validates the query", {
  expect_equal(nrow(semantic_search("q", character(0), model = MockEmbed$new())), 0)
  expect_error(semantic_search("", c("a"), model = MockEmbed$new()), "non-empty")
})

test_that("cosine_similarity handles identical, orthogonal, and zero vectors", {
  expect_equal(aisdk:::cosine_similarity(c(1, 2, 3), c(1, 2, 3)), 1)
  expect_equal(aisdk:::cosine_similarity(c(1, 0), c(0, 1)), 0)
  expect_equal(aisdk:::cosine_similarity(c(0, 0), c(1, 1)), 0) # zero vector -> 0, not NaN
})
