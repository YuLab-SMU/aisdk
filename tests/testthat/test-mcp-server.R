# Test MCP Server

test_that("McpServer can be created", {
  server <- McpServer$new("test-server", "1.0.0")
  
  expect_s3_class(server, "McpServer")
  expect_equal(server$name, "test-server")
  expect_equal(server$version, "1.0.0")
  expect_type(server$tools, "list")
  expect_length(server$tools, 0)
})

test_that("create_mcp_server convenience function works", {
  server <- create_mcp_server("my-server", "2.0.0")
  
  expect_s3_class(server, "McpServer")
  expect_equal(server$name, "my-server")
})

test_that("McpServer can add tools", {
  server <- McpServer$new("test-server")
  
  test_tool <- tool(
    name = "test_tool",
    description = "A test tool",
    parameters = z_object(x = z_string()),
    execute = function(args) paste("Got:", args$x)
  )
  
  server$add_tool(test_tool)
  
  expect_length(server$tools, 1)
  expect_equal(server$tools$test_tool$name, "test_tool")
})

test_that("McpServer rejects non-Tool objects", {
  server <- McpServer$new("test-server")
  
  expect_error(server$add_tool("not a tool"))
  expect_error(server$add_tool(list(name = "fake")))
})

test_that("McpServer can add resources", {
  server <- McpServer$new("test-server")
  
  server$add_resource(
    uri = "file:///test.txt",
    name = "Test File",
    description = "A test file",
    mime_type = "text/plain",
    read_fn = function() "file contents"
  )
  
  expect_length(server$resources, 1)
  expect_equal(server$resources[["file:///test.txt"]]$name, "Test File")
})

test_that("McpServer handles initialize request", {
  server <- McpServer$new("test-server", "1.0.0")
  
  req <- '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"test"}},"id":1}'
  resp <- server$process_message(req)
  
  expect_equal(resp$jsonrpc, "2.0")
  expect_equal(resp$id, 1)
  expect_equal(resp$result$serverInfo$name, "test-server")
  expect_equal(resp$result$protocolVersion, "2024-11-05")
})

test_that("McpServer handles tools/list request", {
  server <- McpServer$new("test-server")
  
  test_tool <- tool(
    name = "greet",
    description = "Say hello",
    parameters = z_object(name = z_string()),
    execute = function(args) paste("Hello,", args$name)
  )
  server$add_tool(test_tool)
  
  req <- '{"jsonrpc":"2.0","method":"tools/list","id":2}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 2)
  expect_length(resp$result$tools, 1)
  expect_equal(resp$result$tools[[1]]$name, "greet")
})

test_that("McpServer handles tools/call request", {
  server <- McpServer$new("test-server")
  
  test_tool <- tool(
    name = "echo",
    description = "Echo input",
    parameters = z_object(message = z_string()),
    execute = function(args) args$message
  )
  server$add_tool(test_tool)
  
  req <- '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"echo","arguments":{"message":"hello"}},"id":3}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 3)
  expect_equal(resp$result$content[[1]]$type, "text")
  expect_equal(resp$result$content[[1]]$text, "hello")
})

test_that("McpServer handles tools/call error gracefully", {
  server <- McpServer$new("test-server")
  
  error_tool <- tool(
    name = "error_tool",
    description = "Always errors",
    parameters = z_object(dummy = z_string(description = "unused")),
    execute = function(args) stop("Intentional error")
  )
  server$add_tool(error_tool)
  
  req <- '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"error_tool","arguments":{}},"id":4}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 4)
  expect_true(resp$result$isError)
  expect_true(grepl("Error:", resp$result$content[[1]]$text))
})

test_that("McpServer handles unknown tool", {
  server <- McpServer$new("test-server")
  
  req <- '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"nonexistent","arguments":{}},"id":5}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 5)
  expect_equal(resp$error$code, -32603)  # Internal error
  expect_true(grepl("Unknown tool", resp$error$message))
})

test_that("McpServer handles resources/list request", {
  server <- McpServer$new("test-server")
  
  server$add_resource(
    uri = "file:///data.csv",
    name = "Data",
    description = "Sample data",
    mime_type = "text/csv",
    read_fn = function() "a,b,c"
  )
  
  req <- '{"jsonrpc":"2.0","method":"resources/list","id":6}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 6)
  expect_length(resp$result$resources, 1)
  expect_equal(resp$result$resources[[1]]$uri, "file:///data.csv")
})

test_that("McpServer handles resources/read request", {
  server <- McpServer$new("test-server")
  
  server$add_resource(
    uri = "file:///hello.txt",
    name = "Hello",
    description = "Hello file",
    read_fn = function() "Hello, World!"
  )
  
  req <- '{"jsonrpc":"2.0","method":"resources/read","params":{"uri":"file:///hello.txt"},"id":7}'
  resp <- server$process_message(req)
  
  expect_equal(resp$id, 7)
  expect_equal(resp$result$contents[[1]]$text, "Hello, World!")
  expect_equal(resp$result$contents[[1]]$uri, "file:///hello.txt")
})

test_that("McpServer handles parse error", {
  server <- McpServer$new("test-server")
  
  resp <- server$process_message("not valid json {{{")
  
  expect_equal(resp$error$code, -32700)  # Parse error
})

test_that("McpServer handles method not found", {
  server <- McpServer$new("test-server")
  
  req <- '{"jsonrpc":"2.0","method":"unknown/method","id":8}'
  resp <- server$process_message(req)
  
  expect_equal(resp$error$code, -32601)  # Method not found
})

test_that("McpServer handles notifications (no response)", {
  server <- McpServer$new("test-server")
  
  req <- '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  resp <- server$process_message(req)
  
  expect_null(resp)
})
