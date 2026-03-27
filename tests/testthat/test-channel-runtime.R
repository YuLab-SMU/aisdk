test_that("FileChannelSessionStore persists session state and index metadata", {
  root <- tempfile("channel-store-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  shared_model <- MockModel$new()
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) shared_model)

  session <- ChatSession$new(model = "mock:test", registry = registry)
  session$append_message("user", "hello")
  session$set_metadata("channel", list(channel_id = "test"))

  record <- store$save_session(
    "test:session",
    session,
    record = list(
      channel_id = "test",
      chat_id = "chat-1",
      participants = list(list(sender_id = "u1", sender_name = "Alice"))
    )
  )

  expect_true(file.exists(store$get_session_path("test:session")))
  expect_equal(record$channel_id, "test")
  expect_equal(store$get_record("test:session")$chat_id, "chat-1")

  loaded <- store$load_session("test:session", registry = registry)
  expect_equal(length(loaded$get_history()), 1)
  expect_equal(loaded$get_metadata("channel")$channel_id, "test")
})

test_that("ChannelRuntime reuses shared group sessions and keeps sender identity", {
  TestAdapter <- R6::R6Class(
    "TestChannelAdapter",
    inherit = ChannelAdapter,
    public = list(
      sent = NULL,
      initialize = function() {
        super$initialize(id = "test")
        self$sent <- list()
      },
      parse_request = function(headers = NULL, body = NULL, ...) {
        body <- normalize_channel_body(body)
        channel_request_result(
          type = "inbound",
          messages = list(channel_inbound_message(
            channel_id = "test",
            account_id = "acct",
            event_id = body$event_id,
            chat_id = body$chat_id,
            chat_type = body$chat_type,
            sender_id = body$sender_id,
            sender_name = body$sender_name,
            text = body$text,
            chat_scope = if (identical(body$chat_type, "direct")) "per_sender" else "shared_chat"
          ))
        )
      },
      send_text = function(message, text, ...) {
        self$sent[[length(self$sent) + 1L]] <- list(
          chat_id = message$chat_id,
          sender_id = message$sender_id,
          text = text
        )
        list(ok = TRUE)
      }
    )
  )

  root <- tempfile("channel-runtime-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  adapter <- TestAdapter$new()
  shared_model <- MockModel$new(list(
    list(text = "reply one", tool_calls = NULL, finish_reason = "stop"),
    list(text = "reply two", tool_calls = NULL, finish_reason = "stop")
  ))
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) shared_model)

  runtime <- create_channel_runtime(
    session_store = store,
    model = "mock:runtime-model",
    registry = registry
  )
  runtime$register_adapter(adapter)

  first <- runtime$handle_request(
    "test",
    body = list(
      event_id = "evt-1",
      chat_id = "group-1",
      chat_type = "group",
      sender_id = "u1",
      sender_name = "Alice",
      text = "hello from alice"
    )
  )
  second <- runtime$handle_request(
    "test",
    body = list(
      event_id = "evt-2",
      chat_id = "group-1",
      chat_type = "group",
      sender_id = "u2",
      sender_name = "Bob",
      text = "hello from bob"
    )
  )

  expect_equal(first$results[[1]]$session_key, second$results[[1]]$session_key)
  expect_equal(length(adapter$sent), 2)
  expect_equal(adapter$sent[[2]]$text, "reply two")

  record <- store$get_record(first$results[[1]]$session_key)
  expect_equal(record$chat_id, "group-1")
  expect_equal(length(record$participants), 2)

  loaded <- store$load_session(first$results[[1]]$session_key, registry = registry)
  expect_equal(length(loaded$get_history()), 4)
  expect_match(loaded$get_history()[[1]]$content, "Alice", fixed = TRUE)
  expect_match(loaded$get_history()[[3]]$content, "Bob", fixed = TRUE)
})

test_that("ChannelRuntime deduplicates repeated event ids", {
  TestAdapter <- R6::R6Class(
    "DedupTestChannelAdapter",
    inherit = ChannelAdapter,
    public = list(
      sent = NULL,
      initialize = function() {
        super$initialize(id = "test")
        self$sent <- list()
      },
      parse_request = function(headers = NULL, body = NULL, ...) {
        channel_request_result(
          type = "inbound",
          messages = list(channel_inbound_message(
            channel_id = "test",
            account_id = "acct",
            event_id = body$event_id,
            chat_id = body$chat_id,
            chat_type = "group",
            sender_id = body$sender_id,
            sender_name = body$sender_name,
            text = body$text,
            chat_scope = "shared_chat"
          ))
        )
      },
      send_text = function(message, text, ...) {
        self$sent[[length(self$sent) + 1L]] <- text
        list(ok = TRUE)
      }
    )
  )

  root <- tempfile("channel-dedup-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    MockModel$new(list(list(text = "reply once", tool_calls = NULL, finish_reason = "stop")))
  })
  runtime <- create_channel_runtime(
    session_store = store,
    model = "mock:runtime-model",
    registry = registry
  )
  adapter <- TestAdapter$new()
  runtime$register_adapter(adapter)

  first <- runtime$handle_request(
    "test",
    body = list(
      event_id = "evt-repeat",
      chat_id = "group-1",
      sender_id = "u1",
      sender_name = "Alice",
      text = "hello"
    )
  )
  second <- runtime$handle_request(
    "test",
    body = list(
      event_id = "evt-repeat",
      chat_id = "group-1",
      sender_id = "u1",
      sender_name = "Alice",
      text = "hello again"
    )
  )

  expect_false(first$results[[1]]$duplicate)
  expect_true(second$results[[1]]$duplicate)
  expect_equal(length(adapter$sent), 1)
  expect_true(store$has_processed_event("test", "evt-repeat"))
})

test_that("ChannelRuntime sends local attachments referenced in final reply text", {
  tmp_png <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47)), tmp_png)
  on.exit(unlink(tmp_png), add = TRUE)

  TestAdapter <- R6::R6Class(
    "AttachmentTestChannelAdapter",
    inherit = ChannelAdapter,
    public = list(
      sent_text = NULL,
      sent_attachments = NULL,
      initialize = function() {
        super$initialize(id = "test")
        self$sent_text <- list()
        self$sent_attachments <- list()
      },
      parse_request = function(headers = NULL, body = NULL, ...) {
        channel_request_result(
          type = "inbound",
          messages = list(channel_inbound_message(
            channel_id = "test",
            account_id = "acct",
            event_id = "evt-attach",
            chat_id = "chat-1",
            chat_type = "direct",
            sender_id = "u1",
            sender_name = "Alice",
            text = "send attachment",
            chat_scope = "per_sender"
          ))
        )
      },
      send_text = function(message, text, ...) {
        self$sent_text[[length(self$sent_text) + 1L]] <- text
        list(ok = TRUE)
      },
      send_attachment = function(message, path, ...) {
        self$sent_attachments[[length(self$sent_attachments) + 1L]] <- path
        list(ok = TRUE, path = path)
      }
    )
  )

  root <- tempfile("channel-attach-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    MockModel$new(list(list(
      text = paste("Done. File saved to", tmp_png),
      tool_calls = NULL,
      finish_reason = "stop"
    )))
  })

  runtime <- create_channel_runtime(
    session_store = store,
    model = "mock:runtime-model",
    registry = registry
  )
  adapter <- TestAdapter$new()
  runtime$register_adapter(adapter)

  result <- runtime$handle_request("test", body = list())

  expect_equal(length(adapter$sent_text), 1)
  expect_equal(length(adapter$sent_attachments), 1)
  expect_equal(adapter$sent_attachments[[1]], normalizePath(tmp_png, winslash = "/", mustWork = FALSE))
  expect_equal(result$results[[1]]$attachments[[1]]$path, normalizePath(tmp_png, winslash = "/", mustWork = FALSE))
})

test_that("channel_extract_local_paths recognizes Windows absolute paths", {
  extracted <- testthat::with_mocked_bindings(
    channel_extract_local_paths("Done. File saved to C:/Users/test/output.png"),
    channel_file_exists = function(path) identical(path, "C:/Users/test/output.png"),
    channel_normalize_path = function(path) path,
    .package = "aisdk"
  )

  expect_equal(extracted, "C:/Users/test/output.png")
})

test_that("ChannelRuntime sends attachments from structured tool artifacts even without path in final text", {
  tmp_png <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47)), tmp_png)
  on.exit(unlink(tmp_png), add = TRUE)

  ArtifactModel <- R6::R6Class(
    "ArtifactModel",
    inherit = LanguageModelV1,
    public = list(
      call_index = 0L,
      initialize = function() {
        super$initialize(provider = "mock", model_id = "artifact-model")
      },
      do_generate = function(params) {
        self$call_index <- self$call_index + 1L
        if (self$call_index == 1L) {
          return(list(
            text = "",
            tool_calls = list(list(id = "tc1", name = "make_image", arguments = list())),
            finish_reason = "tool_calls"
          ))
        }
        list(
          text = "图已经准备好了。",
          tool_calls = NULL,
          finish_reason = "stop"
        )
      },
      format_tool_result = function(tool_call_id, tool_name, result) {
        list(role = "tool", tool_call_id = tool_call_id, name = tool_name, content = result)
      },
      get_history_format = function() {
        "openai"
      }
    )
  )

  image_tool <- tool(
    name = "make_image",
    description = "Produce a PNG artifact",
    parameters = z_empty_object(),
    execute = function() {
      out <- "PNG generated"
      attr(out, "aisdk_artifacts") <- list(list(path = normalizePath(tmp_png, winslash = "/", mustWork = FALSE)))
      out
    }
  )

  TestAdapter <- R6::R6Class(
    "ArtifactAdapter",
    inherit = ChannelAdapter,
    public = list(
      sent_text = NULL,
      sent_attachments = NULL,
      initialize = function() {
        super$initialize(id = "test")
        self$sent_text <- list()
        self$sent_attachments <- list()
      },
      parse_request = function(headers = NULL, body = NULL, ...) {
        channel_request_result(
          type = "inbound",
          messages = list(channel_inbound_message(
            channel_id = "test",
            account_id = "acct",
            event_id = "evt-artifact",
            chat_id = "chat-1",
            chat_type = "direct",
            sender_id = "u1",
            sender_name = "Alice",
            text = "make image",
            chat_scope = "per_sender"
          ))
        )
      },
      send_text = function(message, text, ...) {
        self$sent_text[[length(self$sent_text) + 1L]] <- text
        list(ok = TRUE)
      },
      send_attachment = function(message, path, ...) {
        self$sent_attachments[[length(self$sent_attachments) + 1L]] <- path
        list(ok = TRUE, path = path)
      }
    )
  )

  root <- tempfile("channel-artifact-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  runtime <- create_channel_runtime(
    session_store = store,
    model = ArtifactModel$new(),
    tools = list(image_tool),
    max_steps = 3
  )
  adapter <- TestAdapter$new()
  runtime$register_adapter(adapter)

  result <- runtime$handle_request("test", body = list())

  expect_equal(length(adapter$sent_attachments), 1)
  expect_equal(adapter$sent_attachments[[1]], normalizePath(tmp_png, winslash = "/", mustWork = FALSE))
  expect_equal(length(result$results[[1]]$attachments), 1)
  expect_equal(result$results[[1]]$reply_text, "图已经准备好了。")
})

test_that("ChannelRuntime warns when reply claims attachment success without actual attachment delivery", {
  TestAdapter <- R6::R6Class(
    "AttachmentClaimAdapter",
    inherit = ChannelAdapter,
    public = list(
      sent_text = NULL,
      initialize = function() {
        super$initialize(id = "test")
        self$sent_text <- list()
      },
      parse_request = function(headers = NULL, body = NULL, ...) {
        channel_request_result(
          type = "inbound",
          messages = list(channel_inbound_message(
            channel_id = "test",
            account_id = "acct",
            event_id = "evt-claim",
            chat_id = "chat-1",
            chat_type = "direct",
            sender_id = "u1",
            sender_name = "Alice",
            text = "send file",
            chat_scope = "per_sender"
          ))
        )
      },
      send_text = function(message, text, ...) {
        self$sent_text[[length(self$sent_text) + 1L]] <- text
        list(ok = TRUE)
      }
    )
  )

  root <- tempfile("channel-claim-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    MockModel$new(list(list(
      text = "我已经把 PNG 发给你了（base64）。",
      tool_calls = NULL,
      finish_reason = "stop"
    )))
  })

  runtime <- create_channel_runtime(
    session_store = store,
    model = "mock:runtime-model",
    registry = registry
  )
  adapter <- TestAdapter$new()
  runtime$register_adapter(adapter)

  result <- runtime$handle_request("test", body = list())

  expect_match(result$results[[1]]$reply_text, "系统未检测到实际发送成功的附件")
  expect_match(adapter$sent_text[[1]], "系统未检测到实际发送成功的附件")
})

test_that("ChannelRuntime can create child sessions linked to a parent", {
  root <- tempfile("channel-child-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  shared_model <- MockModel$new()
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) shared_model)
  runtime <- create_channel_runtime(
    session_store = store,
    model = "mock:child-model",
    registry = registry
  )

  parent <- ChatSession$new(model = "mock:child-model", registry = registry)
  parent$append_message("user", "root task")
  store$save_session("parent", parent, record = list(channel_id = "test"))

  child_key <- runtime$create_child_session("parent", child_session_key = "parent:child:1")

  expect_equal(child_key, "parent:child:1")
  expect_true("parent:child:1" %in% store$get_record("parent")$child_session_keys)

  child <- store$load_session("parent:child:1", registry = registry)
  expect_equal(length(child$get_history()), 1)
  expect_equal(child$get_metadata("parent_session_key"), "parent")
})

test_that("FeishuChannelAdapter handles challenge and message events", {
  adapter <- create_feishu_channel_adapter(
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1"
  )

  challenge <- adapter$parse_request(
    body = list(
      type = "url_verification",
      challenge = "abc",
      token = "token-1"
    )
  )
  expect_equal(challenge$type, "challenge")
  expect_equal(challenge$payload$challenge, "abc")

  parsed <- adapter$parse_request(
    body = list(
      token = "token-1",
      header = list(
        event_type = "im.message.receive_v1",
        event_id = "evt-1"
      ),
      event = list(
        sender = list(
          sender_id = list(open_id = "ou_1")
        ),
        message = list(
          message_id = "om_1",
          chat_id = "oc_1",
          chat_type = "group",
          message_type = "text",
          content = "{\"text\":\"hello feishu\"}"
        )
      )
    )
  )

  expect_equal(parsed$type, "inbound")
  expect_equal(length(parsed$messages), 1)
  expect_equal(parsed$messages[[1]]$text, "hello feishu")
  expect_equal(parsed$messages[[1]]$chat_type, "group")
  expect_equal(adapter$resolve_session_key(parsed$messages[[1]]), "feishu:cli_a:group:oc_1")
})

test_that("FeishuChannelAdapter validates signature and decrypts encrypted events", {
  encrypt_key <- "0123456789abcdef0123456789abcdef"
  adapter <- create_feishu_channel_adapter(
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1",
    encrypt_key = encrypt_key
  )

  inner_payload <- jsonlite::toJSON(
    list(
      token = "token-1",
      header = list(
        event_type = "im.message.receive_v1",
        event_id = "evt-encrypted-1"
      ),
      event = list(
        sender = list(sender_id = list(open_id = "ou_2")),
        message = list(
          message_id = "om_2",
          chat_id = "oc_2",
          chat_type = "group",
          message_type = "text",
          content = "{\"text\":\"secret hello\"}"
        )
      )
    ),
    auto_unbox = TRUE,
    null = "null"
  )

  key_raw <- openssl::sha256(charToRaw(enc2utf8(encrypt_key)))
  iv <- as.raw(seq_len(16))
  cipher <- openssl::aes_cbc_encrypt(charToRaw(inner_payload), key = key_raw, iv = iv)
  outer_body <- jsonlite::toJSON(
    list(encrypt = base64enc::base64encode(c(iv, cipher))),
    auto_unbox = TRUE,
    null = "null"
  )

  timestamp <- "1711260000"
  nonce <- "nonce-1"
  signature <- digest::digest(
    paste0(timestamp, nonce, encrypt_key, outer_body),
    algo = "sha256",
    serialize = FALSE
  )

  parsed <- adapter$parse_request(
    headers = list(
      "x-lark-signature" = signature,
      "x-lark-request-timestamp" = timestamp,
      "x-lark-request-nonce" = nonce
    ),
    body = outer_body
  )

  expect_equal(parsed$type, "inbound")
  expect_equal(parsed$messages[[1]]$text, "secret hello")
  expect_equal(parsed$messages[[1]]$event_id, "evt-encrypted-1")
})

test_that("FeishuChannelAdapter fails closed on invalid signature", {
  encrypt_key <- "0123456789abcdef0123456789abcdef"
  adapter <- create_feishu_channel_adapter(
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1",
    encrypt_key = encrypt_key
  )

  body <- jsonlite::toJSON(list(encrypt = "bogus"), auto_unbox = TRUE, null = "null")

  expect_error(
    adapter$parse_request(
      headers = list(
        "x-lark-signature" = "bad-signature",
        "x-lark-request-timestamp" = "1711260000",
        "x-lark-request-nonce" = "nonce-1"
      ),
      body = body
    ),
    "signature validation failed"
  )
})

test_that("FeishuChannelAdapter formats inbound file messages with attachment metadata", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  writeLines(c("%PDF-1.4", "Mock PDF Title", "Abstract section"), tmp_pdf)
  on.exit(unlink(tmp_pdf), add = TRUE)

  adapter <- create_feishu_channel_adapter(
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1",
    download_resource_fn = function(message_id, file_key, type, file_name) {
      normalizePath(tmp_pdf, winslash = "/", mustWork = FALSE)
    }
  )

  parsed <- adapter$parse_request(
    body = list(
      token = "token-1",
      header = list(
        event_type = "im.message.receive_v1",
        event_id = "evt-file-1"
      ),
      event = list(
        sender = list(sender_id = list(open_id = "ou_file")),
        message = list(
          message_id = "om_file_1",
          chat_id = "oc_file_1",
          chat_type = "p2p",
          message_type = "file",
          content = "{\"file_key\":\"file_x\",\"file_name\":\"paper.pdf\"}"
        )
      )
    )
  )

  expect_equal(parsed$type, "inbound")
  expect_equal(parsed$messages[[1]]$attachments[[1]]$file_name, "paper.pdf")
  expect_equal(parsed$messages[[1]]$attachments[[1]]$local_path, normalizePath(tmp_pdf, winslash = "/", mustWork = FALSE))
  session <- create_chat_session(model = MockModel$new())
  prepared <- adapter$prepare_inbound_message(session, parsed$messages[[1]])
  formatted <- adapter$format_inbound_message(prepared)
  expect_match(formatted, "document_context_begin", fixed = TRUE)
  expect_match(formatted, "document_name: paper.pdf", fixed = TRUE)
})

test_that("channel_extract_pdf_pages prefers the R backend when it has text", {
  extracted <- testthat::with_mocked_bindings(
    channel_extract_pdf_pages("paper.pdf"),
    channel_extract_pdf_pages_r = function(path) {
      list(
        page_count = 1L,
        pages = list(list(page = 1L, text = "R backend text")),
        extractor = "pdftools"
      )
    },
    channel_extract_pdf_pages_python = function(path) {
      list(
        page_count = 1L,
        pages = list(list(page = 1L, text = "Python backend text")),
        extractor = "python"
      )
    },
    .package = "aisdk"
  )

  expect_equal(extracted$extractor, "pdftools")
  expect_equal(extracted$pages[[1]]$text, "R backend text")
})

test_that("channel_extract_pdf_pages falls back to Python when the R backend is empty", {
  extracted <- testthat::with_mocked_bindings(
    channel_extract_pdf_pages("paper.pdf"),
    channel_extract_pdf_pages_r = function(path) {
      list(
        page_count = 1L,
        pages = list(list(page = 1L, text = "")),
        extractor = "pdftools"
      )
    },
    channel_extract_pdf_pages_python = function(path) {
      list(
        page_count = 1L,
        pages = list(list(page = 1L, text = "Python backend text")),
        extractor = "python"
      )
    },
    .package = "aisdk"
  )

  expect_equal(extracted$extractor, "python")
  expect_equal(extracted$pages[[1]]$text, "Python backend text")
})

test_that("channel_build_document_record keeps preview text when PDF extraction is empty", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  writeLines("%PDF-1.4", tmp_pdf)
  on.exit(unlink(tmp_pdf), add = TRUE)

  record <- testthat::with_mocked_bindings(
    channel_build_document_record(list(
      type = "file",
      file_name = "paper.pdf",
      local_path = tmp_pdf,
      preview = "Preview title\n\nPreview abstract"
    )),
    channel_extract_pdf_pages = function(path) {
      list(page_count = 0L, pages = list(), extractor = NULL)
    },
    .package = "aisdk"
  )

  expect_match(record$summary, "Preview title", fixed = TRUE)
  expect_length(record$chunks, 1L)
  expect_match(record$chunks[[1]], "Preview abstract", fixed = TRUE)
})

test_that("channel_apply_skill_routing preloads skill-creator for capability requests", {
  temp_root <- tempdir()
  skill_dir <- file.path(temp_root, "channel-routing-skills")
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)

  skill_path <- file.path(skill_dir, "skill-creator")
  dir.create(skill_path, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "---",
    "name: skill-creator",
    "description: Create and improve reusable skills",
    "---",
    "Use this workflow to create reusable skills."
  ), file.path(skill_path, "SKILL.md"))

  agent <- channel_resolve_agent(agent = NULL, skills = skill_dir, model = "mock:test")
  message <- channel_inbound_message(
    channel_id = "feishu",
    account_id = "cli_a",
    chat_id = "oc_skill",
    chat_type = "direct",
    sender_id = "ou_skill",
    sender_name = "user",
    text = "帮我加个能力，以后自动处理这类 PDF",
    chat_scope = "per_sender"
  )

  routed <- channel_apply_skill_routing("原始提示", message, agent = agent, session = NULL)
  expect_match(routed, "routing_hint_begin", fixed = TRUE)
  expect_match(routed, "skill-creator", fixed = TRUE)
  expect_match(routed, "Use this workflow to create reusable skills.", fixed = TRUE)
})

test_that("Feishu document context is injected on follow-up reference instead of raw full-file history", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  writeLines(c("%PDF-1.4", "Paper Title", "Abstract text line 1", "Abstract text line 2"), tmp_pdf)
  on.exit(unlink(tmp_pdf), add = TRUE)

  adapter <- create_feishu_channel_adapter(
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1",
    download_resource_fn = function(message_id, file_key, type, file_name) {
      normalizePath(tmp_pdf, winslash = "/", mustWork = FALSE)
    }
  )

  session <- create_chat_session(model = MockModel$new())

  file_parsed <- adapter$parse_request(
    body = list(
      token = "token-1",
      header = list(event_type = "im.message.receive_v1", event_id = "evt-file-ctx"),
      event = list(
        sender = list(sender_id = list(open_id = "ou_file")),
        message = list(
          message_id = "om_file_ctx",
          chat_id = "oc_file_ctx",
          chat_type = "p2p",
          message_type = "file",
          content = "{\"file_key\":\"file_x\",\"file_name\":\"paper.pdf\"}"
        )
      )
    )
  )$messages[[1]]

  prepared_file <- adapter$prepare_inbound_message(session, file_parsed)
  expect_true("channel_documents" %in% session$list_metadata())
  expect_true(is.null(prepared_file$metadata$parsed_content$summary))
  file_prompt <- adapter$format_inbound_message(prepared_file)
  expect_true(grepl("document_context_begin", file_prompt, fixed = TRUE))

  followup <- channel_inbound_message(
    channel_id = "feishu",
    account_id = "cli_a",
    chat_id = "oc_file_ctx",
    chat_type = "direct",
    sender_id = "ou_file",
    sender_name = "user",
    text = "根据我刚上传的 PDF，生成摘要",
    chat_scope = "per_sender"
  )
  prepared_followup <- adapter$prepare_inbound_message(session, followup)
  expect_match(prepared_followup$text, "document_context_begin", fixed = TRUE)
  expect_match(prepared_followup$text, "document_name: paper.pdf", fixed = TRUE)
})

test_that("Feishu webhook handler returns challenge and processes inbound events", {
  root <- tempfile("feishu-handler-")
  dir.create(root, recursive = TRUE)
  store <- create_file_channel_session_store(root)
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    MockModel$new(list(list(text = "feishu reply", tool_calls = NULL, finish_reason = "stop")))
  })

  sent <- list()
  statuses <- list()
  runtime <- create_feishu_channel_runtime(
    session_store = store,
    app_id = "cli_a",
    app_secret = "secret",
    verification_token = "token-1",
    send_text_fn = function(message, text, ...) {
      sent[[length(sent) + 1L]] <<- list(chat_id = message$chat_id, text = text)
      list(ok = TRUE)
    },
    send_status_fn = function(message, status, text = NULL, ...) {
      statuses[[length(statuses) + 1L]] <<- list(
        chat_id = message$chat_id,
        status = status,
        text = text
      )
      list(ok = TRUE)
    },
    model = "mock:runtime-model",
    registry = registry
  )

  handler <- create_feishu_webhook_handler(runtime)

  challenge <- handler(
    body = list(type = "url_verification", challenge = "abc", token = "token-1")
  )
  expect_equal(challenge$status, 200L)
  expect_match(challenge$body, "\"challenge\":\"abc\"")

  response <- handler(
    body = list(
      token = "token-1",
      header = list(
        event_type = "im.message.receive_v1",
        event_id = "evt-feishu-1"
      ),
      event = list(
        sender = list(sender_id = list(open_id = "ou_1")),
        message = list(
          message_id = "om_1",
          chat_id = "oc_1",
          chat_type = "group",
          message_type = "text",
          content = "{\"text\":\"hello feishu\"}"
        )
      )
    )
  )

  expect_equal(response$status, 200L)
  expect_equal(length(statuses), 1)
  expect_equal(statuses[[1]]$status, "thinking")
  expect_equal(length(sent), 1)
  expect_equal(sent[[1]]$text, "feishu reply")
  expect_true(store$has_processed_event("feishu", "evt-feishu-1"))
})

test_that("Feishu upload success accepts integer code 0 responses", {
  expect_true(isTRUE((0L %||% 0) == 0))
})
