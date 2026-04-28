test_that("classify_context_regime respects default thresholds", {
  expect_equal(aisdk:::classify_context_regime(0.10), "green")
  expect_equal(aisdk:::classify_context_regime(0.40), "yellow")
  expect_equal(aisdk:::classify_context_regime(0.70), "orange")
  expect_equal(aisdk:::classify_context_regime(0.90), "red")
  expect_equal(aisdk:::classify_context_regime(NA_real_), "unknown")
})

test_that("get_session_context_metrics estimates occupancy for known models", {
  session <- aisdk::create_chat_session(model = "openai:gpt-5-mini")
  session$append_message("user", "hello world")

  metrics <- aisdk:::get_session_context_metrics(session)

  expect_equal(metrics$provider, "openai")
  expect_equal(metrics$model_id, "gpt-5-mini")
  expect_equal(metrics$context_window, 128000L)
  expect_true(metrics$used_tokens > 0)
  expect_equal(metrics$regime, "green")
})

test_that("get_session_context_metrics uses DeepSeek V4 million-token metadata", {
  session <- aisdk::create_chat_session(model = "deepseek:deepseek-v4-flash")
  session$append_message("user", "hello world")

  metrics <- aisdk:::get_session_context_metrics(session)

  expect_equal(metrics$provider, "deepseek")
  expect_equal(metrics$model_id, "deepseek-v4-flash")
  expect_equal(metrics$context_window, 1000000L)
  expect_equal(metrics$max_output, 384000L)
  expect_false(metrics$estimated)
})

test_that("DeepSeek V4 fallback context is one million tokens", {
  expect_equal(aisdk:::infer_session_context_window("deepseek", "deepseek-v4"), 1000000L)
  expect_equal(aisdk:::infer_session_context_window("deepseek", "deepseek-v4-pro"), 1000000L)
  expect_equal(aisdk:::infer_session_context_window("deepseek", "deepseek-v4-flash"), 1000000L)
})

test_that("ChatSession context state helpers round-trip", {
  session <- aisdk::create_chat_session(model = MockModel$new())

  state <- session$get_context_state()
  expect_true(is.list(state))
  expect_equal(state$version, 1L)
  expect_equal(state$occupancy$status, "green")

  session$set_context_management_mode("basic")
  expect_equal(session$get_context_management_mode(), "basic")

  session$set_context_state(list(active_facts = list(list(text = "fact-a"))))
  expect_equal(session$get_context_state()$active_facts[[1]]$text, "fact-a")

  session$clear_context_state()
  expect_length(session$get_context_state()$active_facts, 0)
})

test_that("assemble_session_messages compacts older transcript in basic mode", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      context_window_override = 50L
    )
  )

  for (i in seq_len(10)) {
    session$append_message("user", paste(rep(sprintf("user-%02d", i), 5), collapse = " "))
    session$append_message("assistant", paste(rep(sprintf("assistant-%02d", i), 5), collapse = " "))
  }

  assembled <- aisdk:::assemble_session_messages(
    session = session,
    messages = session$get_history(),
    system = "Base system",
    persist = TRUE
  )

  expect_lt(length(assembled$messages), length(session$get_history()))
  expect_match(assembled$system, "\\[COMPACTED CONVERSATION CONTEXT\\]")
  expect_equal(assembled$metrics$regime, "red")

  state <- session$get_context_state()
  expect_true(nzchar(state$rolling_summary))
  expect_true(state$compacted_message_count > 0)
  expect_equal(state$occupancy$status, "red")
})

test_that("ChatSession send uses compacted prompt when context management is enabled", {
  mock_model <- MockModel$new()
  mock_model$add_response(text = "done")

  session <- aisdk::create_chat_session(
    model = mock_model,
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      context_window_override = 50L
    )
  )

  for (i in seq_len(10)) {
    session$append_message("user", paste(rep(sprintf("history-user-%02d", i), 4), collapse = " "))
    session$append_message("assistant", paste(rep(sprintf("history-assistant-%02d", i), 4), collapse = " "))
  }

  full_history_size <- length(session$get_history()) + 1L
  session$send("final request for compaction")

  expect_lt(length(mock_model$last_params$messages), full_history_size)
  expect_match(mock_model$last_params$messages[[1]]$content, "\\[COMPACTED CONVERSATION CONTEXT\\]")

  state <- session$get_context_state()
  expect_true(state$compacted_message_count > 0)
  expect_equal(state$occupancy$status, "red")
})

test_that("refresh_context_state builds live object cards and injects them into assembled prompt", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(mode = "basic"))
  env <- session$get_envir()
  env$demo_df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  state <- session$refresh_context_state()
  expect_true("demo_df" %in% names(state$object_cards))
  expect_match(state$object_cards$demo_df$adapter, "generic")
  expect_match(state$object_cards$demo_df$inspection_hint$full_call, "inspect_r_object", fixed = TRUE)

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "\\[STRUCTURED CONTEXT STATE\\]")
  expect_match(assembled$system, "demo_df", fixed = TRUE)
  expect_match(assembled$system, "Live object cards:", fixed = TRUE)
  expect_match(assembled$system, "deep inspect: inspect_r_object", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve semantic object cards as ranked hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("semantic_objects"),
    retrieval_provider_order = c("semantic_objects")
  ))
  session$set_context_state(list(
    object_cards = list(
      sce = list(
        name = "sce",
        kind = "object",
        class = c("SingleCellExperiment"),
        adapter = "single-cell-experiment",
        summary = "Single-cell object with assays and reduced dimensions",
        workflow_hint = list(steps = c("inspect assays and cell metadata", "quality control", "cluster annotation")),
        accessors = c("assays()", "colData()", "reducedDims()"),
        inspection_hint = list(full_call = "inspect_r_object(name = \"sce\", detail = \"full\")")
      )
    )
  ))
  session$append_message("user", "inspect assays and reduced dimensions")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "semantic_objects", fixed = TRUE)
  expect_match(assembled$system, "sce", fixed = TRUE)
  expect_match(assembled$system, "workflow:", fixed = TRUE)
  expect_match(assembled$system, "accessors:", fixed = TRUE)
})

test_that("refresh_context_state records tool digests and artifact cards from generation results", {
  mock_model <- MockModel$new()
  mock_model$add_response(
    tool_calls = list(list(
      id = "call_1",
      name = "emit_artifact",
      arguments = list()
    ))
  )
  mock_model$add_response(text = "artifact ready")

  emit_tool <- tool(
    name = "emit_artifact",
    description = "Emit a synthetic artifact",
    parameters = z_empty_object(),
    execute = function() {
      out <- "Created artifact summary"
      attr(out, "aisdk_artifacts") <- list(list(path = normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)))
      out
    }
  )

  session <- aisdk::create_chat_session(
    model = mock_model,
    tools = list(emit_tool),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(mode = "basic"))

  session$send("make artifact")
  state <- session$get_context_state()

  expect_true(length(state$tool_digest) >= 1)
  expect_equal(state$tool_digest[[1]]$tool, "emit_artifact")
  expect_true(length(state$artifact_cards) >= 1)

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "Recent tool digests:", fixed = TRUE)
  expect_match(assembled$system, "emit_artifact", fixed = TRUE)
  expect_match(assembled$system, "Recent artifacts:", fixed = TRUE)
})

test_that("inspect_r_object tool results enrich object cards with last inspection preview", {
  mock_model <- MockModel$new()
  mock_model$add_response(
    tool_calls = list(list(
      id = "call_1",
      name = "inspect_r_object",
      arguments = list(name = "demo_df", detail = "full")
    ))
  )
  mock_model$add_response(text = "done")

  session <- aisdk::create_chat_session(
    model = mock_model,
    tools = create_r_context_tools(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(mode = "basic"))
  env <- session$get_envir()
  env$demo_df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  session$send("inspect demo_df deeply")
  state <- session$get_context_state()

  expect_true("demo_df" %in% names(state$object_cards))
  expect_equal(state$object_cards$demo_df$last_inspection$tool, "inspect_r_object")
  expect_equal(state$object_cards$demo_df$last_inspection$detail, "full")
  expect_true(nzchar(state$object_cards$demo_df$last_inspection$preview))

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "last inspection:", fixed = TRUE)
  expect_match(assembled$system, "semantic_objects", fixed = TRUE)
})

test_that("SharedSession context assembly includes carryover state and append-only events", {
  session <- SharedSession$new(model = MockModel$new(), sandbox_mode = "permissive", trace_enabled = TRUE)
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      context_window_override = 80L
    )
  )

  session$push_context("Planner", "Coordinate the subtask", NULL)
  session$push_context("Worker", "Inspect project files", "Planner")
  state <- session$refresh_context_state()

  expect_equal(state$carryover$current_agent, "Worker")
  expect_equal(state$carryover$depth, 2)
  expect_true(length(state$event_log) >= 2)

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "Carryover state:", fixed = TRUE)
  expect_match(assembled$system, "current_agent: Worker", fixed = TRUE)
  expect_match(assembled$system, "Recent context events:", fixed = TRUE)
  expect_match(assembled$system, "task_state", fixed = TRUE)
})

test_that("refresh_context_state synthesizes active facts, decisions, and open loops", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(mode = "basic"))
  env <- session$get_envir()
  env$demo_df <- data.frame(x = 1:3)

  session$append_message("user", "We must avoid rewriting the whole workflow.")
  session$append_message("assistant", "We will use the lightweight path first.")
  session$append_message("assistant", "Next step is wiring retrieval later.")

  state <- session$refresh_context_state()

  expect_true(length(state$active_facts) >= 1)
  expect_true(length(state$decisions) >= 1)
  expect_true(length(state$open_loops) >= 1)

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "Active facts:", fixed = TRUE)
  expect_match(assembled$system, "Decisions:", fixed = TRUE)
  expect_match(assembled$system, "Open loops:", fixed = TRUE)
})

test_that("assemble_session_messages can inject project memory recalls when available", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  project_root <- tempfile("project-memory-root-")
  dir.create(project_root, recursive = TRUE)
  on.exit(unlink(project_root, recursive = TRUE), add = TRUE)

  memory <- project_memory(project_root = project_root)
  memory$store_snippet(
    code = "summarize_results(df)",
    description = "Summarize results table",
    tags = c("summarize", "results"),
    context = "Use this when summarizing results"
  )

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      project_memory_root = project_root
    )
  )
  session$append_message("user", "Can you summarize the results?")

  state <- session$refresh_context_state()
  assembled <- session$assemble_messages()

  snippets <- state$retrieval_cache$snippets
  if (is.null(snippets)) {
    snippets <- list()
  }
  expect_true(length(snippets) >= 1)
  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "project_memory_snippets", fixed = TRUE)
  expect_match(assembled$system, "Summarize results table", fixed = TRUE)
})

test_that("refresh_context_state can merge optional LLM synthesis into working memory", {
  mock_model <- MockModel$new()
  mock_model$add_response(text = '{"active_facts":["LLM fact about the task"],"decisions":["LLM decision to keep it small"],"open_loops":["LLM follow-up item"]}')

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      llm_synthesis = TRUE,
      synthesis_model = mock_model
    )
  )
  session$append_message("user", "We should keep the summary tight.")

  state <- session$refresh_context_state()

  expect_true(any(vapply(state$active_facts, function(item) identical(item$source, "llm_synthesis"), logical(1))))
  expect_true(any(vapply(state$decisions, function(item) identical(item$source, "llm_synthesis"), logical(1))))
  expect_true(any(vapply(state$open_loops, function(item) identical(item$source, "llm_synthesis"), logical(1))))

  assembled <- session$assemble_messages()
  expect_match(assembled$system, "working_memory_llm_enriched", fixed = TRUE)
})

test_that("LLM synthesis policy min_regime can suppress synthesis at low occupancy", {
  mock_model <- MockModel$new()
  mock_model$add_response(text = '{"active_facts":["SHOULD NOT APPEAR"],"decisions":[],"open_loops":[]}')

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      llm_synthesis = TRUE,
      synthesis_model = mock_model,
      llm_synthesis_policy = list(min_regime = "red")
    )
  )
  session$append_message("user", "We should keep the summary tight.")

  state <- session$refresh_context_state()

  expect_false(any(vapply(state$active_facts, function(item) identical(item$text, "SHOULD NOT APPEAR"), logical(1))))
  expect_false(any(vapply(state$event_log, function(event) identical(event$type, "working_memory_llm_enriched"), logical(1))))
})

test_that("LLM synthesis policy controls recent_messages and max_items", {
  capture_env <- new.env(parent = emptyenv())
  capturing_model <- structure(
    list(
      provider = "mock",
      model_id = "context-synth-capture",
      do_generate = function(params) {
        capture_env$last_prompt <- paste(vapply(params$messages, function(m) m$content, character(1)), collapse = "\n---\n")
        list(
          text = '{"active_facts":["fact 1","fact 2","fact 3"],"decisions":["decision 1","decision 2"],"open_loops":["loop 1","loop 2"]}',
          tool_calls = NULL,
          finish_reason = "stop",
          usage = list(total_tokens = 1)
        )
      },
      get_history_format = function() "openai"
    ),
    class = "LanguageModelV1"
  )

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      llm_synthesis = TRUE,
      synthesis_model = capturing_model,
      llm_synthesis_policy = list(recent_messages = 2L, max_items = 1L)
    )
  )
  session$append_message("user", "first message")
  session$append_message("assistant", "second message")
  session$append_message("user", "third message")
  session$append_message("assistant", "fourth message")

  state <- session$refresh_context_state()

  expect_equal(sum(vapply(state$active_facts, function(item) identical(item$source, "llm_synthesis"), logical(1))), 1)
  expect_equal(sum(vapply(state$decisions, function(item) identical(item$source, "llm_synthesis"), logical(1))), 1)
  expect_equal(sum(vapply(state$open_loops, function(item) identical(item$source, "llm_synthesis"), logical(1))), 1)
  expect_false(grepl("first message", capture_env$last_prompt, fixed = TRUE))
  expect_false(grepl("second message", capture_env$last_prompt, fixed = TRUE))
  expect_true(grepl("third message", capture_env$last_prompt, fixed = TRUE))
  expect_true(grepl("fourth message", capture_env$last_prompt, fixed = TRUE))
})

test_that("assemble_session_messages can inject known similar fixes from project memory", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  project_root <- tempfile("project-memory-fix-root-")
  dir.create(project_root, recursive = TRUE)
  on.exit(unlink(project_root, recursive = TRUE), add = TRUE)

  memory <- project_memory(project_root = project_root)
  memory$store_fix(
    original_code = "bad code",
    error = "Error executing tool 'execute_r_code': object 'missing_df' not found",
    fixed_code = "good code",
    fix_description = "Load or create the missing data frame before executing."
  )

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      project_memory_root = project_root
    )
  )
  session$set_context_state(list(
    open_loops = list(list(
      text = "Resolve tool error in `execute_r_code`: Error executing tool 'execute_r_code': object 'missing_df' not found",
      source = "tool_error"
    ))
  ))

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "project_memory_fixes", fixed = TRUE)
  expect_match(assembled$system, "Load or create the missing data frame before executing.", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve matching session memory entries", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("session_memory"),
    retrieval_provider_order = c("session_memory")
  ))
  session$set_memory("analysis_plan", list(step = "Inspect assay counts and summarize cells"))
  session$append_message("user", "Can you inspect the assay counts?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "session_memory", fixed = TRUE)
  expect_match(assembled$system, "analysis_plan", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve matching document context", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("documents"),
    retrieval_provider_order = c("documents")
  ))
  session$set_metadata("channel_documents", list(list(
    file_name = "protocol.pdf",
    summary = "Protocol summary about assay counts and cell QC",
    chunks = list("Assay counts should be inspected before clustering.")
  )))
  session$append_message("user", "Please inspect assay counts before clustering")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "documents", fixed = TRUE)
  expect_match(assembled$system, "protocol.pdf", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve matching compacted transcript segments", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("transcript_segments"),
    retrieval_provider_order = c("transcript_segments")
  ))
  session$set_context_state(list(
    transcript_segments = list(list(
      summary = "Earlier we agreed to inspect assay counts before normalization."
    ))
  ))
  session$append_message("user", "Can you inspect assay counts now?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "transcript_segments", fixed = TRUE)
  expect_match(assembled$system, "inspect assay counts before normalization", fixed = TRUE)
})

test_that("retrieval scoring can outrank provider order", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("documents", "session_memory"),
    retrieval_provider_order = c("documents", "session_memory")
    ,
    retrieval_scoring_policy = list(
      provider_weights = list(documents = 0.5, session_memory = 2.5),
      max_total_results = 4L
    )
  ))
  session$set_memory("analysis_plan", "Inspect assay counts before plotting")
  session$set_metadata("channel_documents", list(list(
    file_name = "protocol.pdf",
    summary = "Protocol summary about assay counts and QC",
    chunks = list("Inspect assay counts before any downstream plotting.")
  )))
  session$append_message("user", "Inspect assay counts before plotting")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_true(length(ranked_lines) >= 2)
  expect_match(ranked_lines[[1]], "session_memory", fixed = TRUE)
  expect_match(ranked_lines[[2]], "documents", fixed = TRUE)
})

test_that("retrieval scoring can use richer semantic features beyond provider weights", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("documents", "session_memory"),
    retrieval_provider_order = c("session_memory", "documents"),
    retrieval_scoring_policy = list(
      provider_weights = list(documents = 1, session_memory = 1),
      title_match_weight = 0.8,
      summary_match_weight = 0.8,
      preview_match_weight = 0.1,
      bigram_match_weight = 0.4,
      coverage_weight = 0.4,
      max_total_results = 4L
    )
  ))
  session$set_memory("analysis_plan", "Inspect counts plotting note")
  session$set_metadata("channel_documents", list(list(
    file_name = "assay-counts-protocol.pdf",
    summary = "Detailed assay counts inspection protocol for downstream plotting",
    chunks = list("Inspect assay counts before plotting and normalization.")
  )))
  session$append_message("user", "Inspect assay counts before plotting")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_true(length(ranked_lines) >= 2)
  expect_match(ranked_lines[[1]], "documents", fixed = TRUE)
})

test_that("task-aware semantic object features can outrank generic memory hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("semantic_objects", "session_memory"),
    retrieval_provider_order = c("session_memory", "semantic_objects"),
    retrieval_scoring_policy = list(
      provider_weights = list(session_memory = 1, semantic_objects = 1),
      workflow_hint_weight = 0.9,
      accessor_match_weight = 0.7,
      semantic_object_bonus = 0.3,
      max_total_results = 4L
    )
  ))
  session$set_memory("analysis_plan", "Inspect assays note")
  session$set_context_state(list(
    object_cards = list(
      sce = list(
        name = "sce",
        kind = "object",
        class = c("SingleCellExperiment"),
        adapter = "single-cell-experiment",
        summary = "Single-cell object with assays and reduced dimensions",
        workflow_hint = list(steps = c("inspect assays and cell metadata", "quality control", "cluster annotation")),
        accessors = c("assays()", "colData()", "reducedDims()"),
        inspection_hint = list(full_call = "inspect_r_object(name = \"sce\", detail = \"full\")")
      )
    )
  ))
  session$append_message("user", "inspect assays and reduced dimensions")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_true(length(ranked_lines) >= 2)
  expect_match(ranked_lines[[1]], "semantic_objects", fixed = TRUE)
})

test_that("task_state retrieval can surface next-step and runtime state signals", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("task_state"),
    retrieval_provider_order = c("task_state")
  ))
  session$set_context_state(list(
    active_facts = list(list(text = "Current model is configured and ready.", source = "carryover")),
    decisions = list(list(text = "We decided to use the lightweight path first.", source = "message:assistant")),
    open_loops = list(list(text = "Next step is to inspect the assay counts.", source = "message:assistant")),
    tool_digest = list(list(tool = "execute_r_code", status = "error", summary = "object not found", timestamp = as.character(Sys.time()))),
    carryover = list(current_agent = "Planner", global_task = "Coordinate the subtask", stack = list(list(agent = "Planner", task = "Coordinate the subtask")))
  ))
  session$append_message("user", "what's next and what failed")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_true(length(ranked_lines) >= 1)
  expect_match(ranked_lines[[1]], "task_state", fixed = TRUE)
  expect_match(assembled$system, "Open loop", fixed = TRUE)
})

test_that("learned retrieval reranking can reorder top hits after deterministic scoring", {
  reranker_env <- new.env(parent = emptyenv())
  reranker_model <- structure(
    list(
      provider = "mock",
      model_id = "retrieval-reranker",
      do_generate = function(params) {
        reranker_env$prompt <- paste(vapply(params$messages, function(m) m$content, character(1)), collapse = "\n---\n")
        list(
          text = '{"ordered_ids":["documents-02","session_memory-01"]}',
          tool_calls = NULL,
          finish_reason = "stop",
          usage = list(total_tokens = 1)
        )
      },
      get_history_format = function() "openai"
    ),
    class = "LanguageModelV1"
  )

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("documents", "session_memory"),
    retrieval_provider_order = c("documents", "session_memory"),
    retrieval_scoring_policy = list(
      provider_weights = list(documents = 0.5, session_memory = 2.5),
      max_total_results = 4L
    ),
    retrieval_reranking = TRUE,
    retrieval_reranking_model = reranker_model,
    retrieval_reranking_policy = list(top_n = 2L)
  ))
  session$set_memory("analysis_plan", "Inspect assay counts before plotting")
  session$set_metadata("channel_documents", list(list(
    file_name = "protocol.pdf",
    summary = "Protocol summary about assay counts and QC",
    chunks = list("Inspect assay counts before any downstream plotting.")
  )))
  session$append_message("user", "Inspect assay counts before plotting")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_true(length(ranked_lines) >= 2)
  expect_match(ranked_lines[[1]], "documents", fixed = TRUE)
  expect_match(ranked_lines[[2]], "session_memory", fixed = TRUE)
  expect_match(assembled$system, "retrieval_reranked", fixed = TRUE)
  expect_true(grepl("documents-02", reranker_env$prompt, fixed = TRUE))
})

test_that("retrieval matching threshold can filter weak transcript matches", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("transcript_segments"),
    retrieval_provider_order = c("transcript_segments"),
    retrieval_min_hits = list(transcript_segments = 2L)
  ))
  session$set_context_state(list(
    transcript_segments = list(list(
      summary = "Earlier we only discussed normalization."
    ))
  ))
  session$append_message("user", "Inspect assay counts now")

  assembled <- session$assemble_messages()
  expect_false(grepl("transcript_segments", assembled$system, fixed = TRUE))
})

test_that("retrieval scoring policy max_total_results truncates ranked hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("session_memory", "documents", "transcript_segments"),
    retrieval_scoring_policy = list(max_total_results = 2L)
  ))
  session$set_memory("analysis_plan", "Inspect assay counts before plotting")
  session$set_memory("analysis_notes", "Inspect assay counts before plotting and summarize")
  session$set_metadata("channel_documents", list(list(
    file_name = "protocol.pdf",
    summary = "Protocol summary about assay counts and QC",
    chunks = list("Inspect assay counts before any downstream plotting.")
  )))
  session$set_context_state(list(
    transcript_segments = list(list(
      summary = "Earlier we agreed to inspect assay counts before normalization."
    ))
  ))
  session$append_message("user", "Inspect assay counts before plotting")

  assembled <- session$assemble_messages()
  ranked_lines <- strsplit(assembled$system, "\n", fixed = TRUE)[[1]]
  ranked_lines <- ranked_lines[grepl("^\\- \\[", ranked_lines)]

  expect_equal(length(ranked_lines), 2)
})

test_that("refresh_context_state can write recovered tool fixes back into project memory", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  project_root <- tempfile("project-memory-writeback-root-")
  dir.create(project_root, recursive = TRUE)
  on.exit(unlink(project_root, recursive = TRUE), add = TRUE)

  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(
    create_context_management_config(
      mode = "basic",
      project_memory_root = project_root
    )
  )

  generation_result <- list(
    all_tool_calls = list(
      list(id = "1", name = "execute_r_code", arguments = list(code = "bad_code()")),
      list(id = "2", name = "execute_r_code", arguments = list(code = "good_code()"))
    ),
    all_tool_results = list(
      list(id = "1", name = "execute_r_code", result = "Error: object 'x' not found", raw_result = "Error: object 'x' not found", is_error = TRUE),
      list(id = "2", name = "execute_r_code", result = "Result: ok", raw_result = "Result: ok", is_error = FALSE)
    )
  )

  state <- session$refresh_context_state(generation_result = generation_result)
  memory <- project_memory(project_root = project_root)
  retrieved <- memory$find_similar_fix("Error: object 'y' not found")

  expect_true(length(state$event_log) > 0)
  expect_true(any(vapply(state$event_log, function(event) identical(event$type, "project_memory_fix_stored"), logical(1))))
  expect_false(is.null(retrieved))
  expect_equal(retrieved$fixed_code, "good_code()")
})
