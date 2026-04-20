test_that("create_context_management_config normalizes values", {
  config <- create_context_management_config(
    mode = "basic",
    llm_synthesis = TRUE,
    llm_synthesis_policy = list(min_regime = "yellow", recent_messages = 4L, max_items = 2L, temperature = 0.1),
    context_window_override = 64000,
    max_output_tokens_override = 4000,
    project_memory_root = tempdir(),
    retrieval_providers = c("semantic_objects", "task_state", "session_memory", "documents"),
    retrieval_provider_order = c("documents", "semantic_objects", "task_state", "session_memory"),
    retrieval_provider_limits = list(session_memory = 1L, semantic_objects = 2L, task_state = 5L),
    retrieval_min_hits = list(documents = 2L, semantic_objects = 1L, task_state = 0L),
    retrieval_scoring_policy = list(
      provider_weights = list(documents = 2),
      title_match_weight = 0.5,
      summary_match_weight = 0.6,
      preview_match_weight = 0.2,
      bigram_match_weight = 0.3,
      coverage_weight = 0.4,
      workflow_hint_weight = 0.7,
      accessor_match_weight = 0.5,
      semantic_object_bonus = 0.3,
      source_kind_bonus = list(documents = 0.2),
      max_total_results = 4L
    ),
    retrieval_reranking = TRUE,
    retrieval_reranking_model = MockModel$new(),
    retrieval_reranking_policy = list(min_regime = "yellow", top_n = 3L, temperature = 0.2)
  )

  expect_equal(config$mode, "basic")
  expect_true(config$llm_synthesis)
  expect_equal(config$llm_synthesis_policy$min_regime, "yellow")
  expect_equal(config$llm_synthesis_policy$recent_messages, 4L)
  expect_equal(config$llm_synthesis_policy$max_items, 2L)
  expect_equal(config$llm_synthesis_policy$temperature, 0.1)
  expect_equal(config$context_window_override, 64000)
  expect_equal(config$max_output_tokens_override, 4000)
  expect_true(nzchar(config$project_memory_root))
  expect_false(config$retrieval_providers$project_memory_snippets)
  expect_true(config$retrieval_providers$semantic_objects)
  expect_true(config$retrieval_providers$task_state)
  expect_true(config$retrieval_providers$session_memory)
  expect_equal(config$retrieval_provider_order[[1]], "documents")
  expect_equal(config$retrieval_provider_limits$session_memory, 1L)
  expect_equal(config$retrieval_provider_limits$semantic_objects, 2L)
  expect_equal(config$retrieval_provider_limits$task_state, 5L)
  expect_equal(config$retrieval_min_hits$documents, 2L)
  expect_equal(config$retrieval_min_hits$semantic_objects, 1L)
  expect_equal(config$retrieval_min_hits$task_state, 0L)
  expect_equal(config$retrieval_scoring_policy$provider_weights$documents, 2)
  expect_equal(config$retrieval_scoring_policy$title_match_weight, 0.5)
  expect_equal(config$retrieval_scoring_policy$summary_match_weight, 0.6)
  expect_equal(config$retrieval_scoring_policy$bigram_match_weight, 0.3)
  expect_equal(config$retrieval_scoring_policy$workflow_hint_weight, 0.7)
  expect_equal(config$retrieval_scoring_policy$accessor_match_weight, 0.5)
  expect_equal(config$retrieval_scoring_policy$semantic_object_bonus, 0.3)
  expect_equal(config$retrieval_scoring_policy$source_kind_bonus$documents, 0.2)
  expect_equal(config$retrieval_scoring_policy$max_total_results, 4L)
  expect_true(config$retrieval_reranking)
  expect_equal(config$retrieval_reranking_policy$min_regime, "yellow")
  expect_equal(config$retrieval_reranking_policy$top_n, 3L)
  expect_equal(config$retrieval_reranking_policy$temperature, 0.2)
})

test_that("set/get_context_management_config round-trip on ChatSession", {
  session <- create_chat_session(model = MockModel$new())
  model_ref <- MockModel$new()
  config <- create_context_management_config(
    mode = "adaptive",
    llm_synthesis = TRUE,
    synthesis_model = model_ref,
    llm_synthesis_policy = list(min_regime = "orange", recent_messages = 5L, max_items = 1L, temperature = 0),
    context_window_override = 32000,
    max_output_tokens_override = 2000,
    project_memory_root = tempdir(),
    retrieval_providers = list(session_memory = TRUE, documents = FALSE, semantic_objects = TRUE, task_state = TRUE),
    retrieval_provider_order = c("semantic_objects", "task_state", "session_memory", "transcript_segments"),
    retrieval_provider_limits = list(session_memory = 2L, semantic_objects = 1L, task_state = 3L),
    retrieval_min_hits = list(session_memory = 2L, semantic_objects = 1L, task_state = 0L),
    retrieval_scoring_policy = list(
      provider_weights = list(session_memory = 3),
      exact_query_bonus = 1.5,
      coverage_weight = 0.8,
      workflow_hint_weight = 0.6,
      source_kind_bonus = list(session_memory = 0.4)
    ),
    retrieval_reranking = TRUE,
    retrieval_reranking_model = model_ref,
    retrieval_reranking_policy = list(min_regime = "orange", top_n = 2L)
  )

  set_context_management_config(session, config)
  retrieved <- get_context_management_config(session)

  expect_equal(retrieved$mode, "adaptive")
  expect_true(retrieved$llm_synthesis)
  expect_identical(retrieved$synthesis_model, model_ref)
  expect_equal(retrieved$llm_synthesis_policy$min_regime, "orange")
  expect_equal(retrieved$llm_synthesis_policy$recent_messages, 5L)
  expect_equal(retrieved$llm_synthesis_policy$max_items, 1L)
  expect_equal(retrieved$context_window_override, 32000)
  expect_equal(retrieved$max_output_tokens_override, 2000)
  expect_true(nzchar(retrieved$project_memory_root))
  expect_true(retrieved$retrieval_providers$session_memory)
  expect_false(retrieved$retrieval_providers$documents)
  expect_true(retrieved$retrieval_providers$task_state)
  expect_true(retrieved$retrieval_providers$semantic_objects)
  expect_equal(retrieved$retrieval_provider_order[[1]], "semantic_objects")
  expect_equal(retrieved$retrieval_provider_limits$session_memory, 2L)
  expect_equal(retrieved$retrieval_provider_limits$semantic_objects, 1L)
  expect_equal(retrieved$retrieval_provider_limits$task_state, 3L)
  expect_equal(retrieved$retrieval_min_hits$session_memory, 2L)
  expect_equal(retrieved$retrieval_min_hits$semantic_objects, 1L)
  expect_equal(retrieved$retrieval_min_hits$task_state, 0L)
  expect_equal(retrieved$retrieval_scoring_policy$provider_weights$session_memory, 3)
  expect_equal(retrieved$retrieval_scoring_policy$exact_query_bonus, 1.5)
  expect_equal(retrieved$retrieval_scoring_policy$coverage_weight, 0.8)
  expect_equal(retrieved$retrieval_scoring_policy$workflow_hint_weight, 0.6)
  expect_equal(retrieved$retrieval_scoring_policy$source_kind_bonus$session_memory, 0.4)
  expect_true(retrieved$retrieval_reranking)
  expect_identical(retrieved$retrieval_reranking_model, model_ref)
  expect_equal(retrieved$retrieval_reranking_policy$min_regime, "orange")
  expect_equal(retrieved$retrieval_reranking_policy$top_n, 2L)
})

test_that("ChatSession context-management config methods delegate correctly", {
  session <- create_chat_session(model = MockModel$new())

  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      context_window_override = 12345
    )
  )

  config <- session$get_context_management_config()
  expect_equal(config$mode, "basic")
  expect_equal(config$context_window_override, 12345)
})
