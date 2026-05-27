test_that("openai_web_search_tool builds the minimal shape", {
  tool <- openai_web_search_tool()
  expect_equal(tool$type, "web_search")
  expect_null(tool$filters)
  expect_null(tool$user_location)
  expect_null(tool$search_context_size)
})

test_that("openai_web_search_tool forwards filters/user_location/context size", {
  tool <- openai_web_search_tool(
    allowed_domains = c("openai.com", "anthropic.com"),
    user_location = list(country = "US", region = "CA", city = "SF", timezone = "America/Los_Angeles"),
    search_context_size = "high"
  )
  expect_equal(tool$filters$allowed_domains, list("openai.com", "anthropic.com"))
  expect_equal(tool$user_location$city, "SF")
  expect_equal(tool$search_context_size, "high")
})

test_that("openai_web_search_tool rejects invalid context size and supports preview type", {
  expect_error(openai_web_search_tool(search_context_size = "extreme"), "search_context_size")
  preview <- openai_web_search_tool(type = "web_search_preview")
  expect_equal(preview$type, "web_search_preview")
})

test_that("openai_file_search_tool requires vector_store_ids and forwards options", {
  expect_error(openai_file_search_tool(character(0)), "non-empty")
  tool <- openai_file_search_tool(
    vector_store_ids = c("vs_a", "vs_b"),
    max_num_results = 10,
    ranking_options = list(ranker = "auto", score_threshold = 0.5)
  )
  expect_equal(tool$type, "file_search")
  expect_equal(tool$vector_store_ids, list("vs_a", "vs_b"))
  expect_equal(tool$max_num_results, 10)
  expect_equal(tool$ranking_options$ranker, "auto")
})

test_that("openai_code_interpreter_tool defaults to auto container and accepts string shortcut", {
  expect_equal(openai_code_interpreter_tool()$container, list(type = "auto"))
  expect_equal(
    openai_code_interpreter_tool(container = "cntr_abc")$container,
    list(type = "cntr_abc")
  )
  custom <- openai_code_interpreter_tool(container = list(type = "auto", memory_limit = "4g"))
  expect_equal(custom$container$memory_limit, "4g")
})

test_that("openai_computer_use_tool builds the preview shape", {
  tool <- openai_computer_use_tool(display_width = 1280, display_height = 800, environment = "browser")
  expect_equal(tool$type, "computer_use_preview")
  expect_identical(tool$display_width, 1280L)
  expect_identical(tool$display_height, 800L)
  expect_equal(tool$environment, "browser")

  bare <- openai_computer_use_tool()
  expect_equal(bare$type, "computer_use_preview")
  expect_null(bare$display_width)
  expect_null(bare$environment)
})

test_that("openai_hosted_mcp_tool requires label and exactly one of server_url/connector_id", {
  expect_error(openai_hosted_mcp_tool(server_label = ""), "non-empty")
  expect_error(
    openai_hosted_mcp_tool(server_label = "x"),
    "Exactly one of"
  )
  expect_error(
    openai_hosted_mcp_tool(server_label = "x", server_url = "u", connector_id = "c"),
    "Exactly one of"
  )
})

test_that("openai_hosted_mcp_tool forwards full payload for a custom server", {
  tool <- openai_hosted_mcp_tool(
    server_label = "dmcp",
    server_url = "https://dmcp-server.deno.dev/sse",
    server_description = "dice rolling",
    allowed_tools = "roll",
    require_approval = "never",
    headers = list(Authorization = "Bearer t")
  )
  expect_equal(tool$type, "mcp")
  expect_equal(tool$server_label, "dmcp")
  expect_equal(tool$server_url, "https://dmcp-server.deno.dev/sse")
  expect_equal(tool$allowed_tools, list("roll"))
  expect_equal(tool$require_approval, "never")
  expect_equal(tool$headers$Authorization, "Bearer t")
  expect_null(tool$connector_id)
})

test_that("openai_hosted_mcp_tool accepts complex filter shapes", {
  tool <- openai_hosted_mcp_tool(
    server_label = "gmail",
    connector_id = "connector_gmail",
    allowed_tools = list(tool_names = c("send_email", "read_email")),
    require_approval = list(always = list(tool_names = c("send_email"))),
    authorization = "oauth_token"
  )
  expect_equal(tool$connector_id, "connector_gmail")
  expect_equal(tool$allowed_tools$tool_names, c("send_email", "read_email"))
  expect_equal(tool$require_approval$always$tool_names, "send_email")
  expect_equal(tool$authorization, "oauth_token")
})

test_that("built-in tools pass through OpenAIResponsesLanguageModel dispatcher untouched", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    tools = list(
      openai_web_search_tool(search_context_size = "medium"),
      openai_file_search_tool("vs_xyz")
    )
  ))

  expect_length(captured_body$tools, 2)
  expect_equal(captured_body$tools[[1]]$type, "web_search")
  expect_equal(captured_body$tools[[1]]$search_context_size, "medium")
  expect_equal(captured_body$tools[[2]]$type, "file_search")
  expect_equal(captured_body$tools[[2]]$vector_store_ids, list("vs_xyz"))
})
