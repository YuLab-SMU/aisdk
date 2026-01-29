# Test MCP Utility Functions

test_that("jsonrpc_request creates valid request", {
  req <- jsonrpc_request("test/method", list(a = 1), id = 1L)
  
  expect_equal(req$jsonrpc, "2.0")
  expect_equal(req$method, "test/method")
  expect_equal(req$params$a, 1)
  expect_equal(req$id, 1L)
})

test_that("jsonrpc_request without params omits params field", {
  req <- jsonrpc_request("test/method", id = 1L)
  
  expect_null(req$params)
  expect_equal(req$method, "test/method")
})

test_that("jsonrpc_request without id creates notification", {
  req <- jsonrpc_request("notifications/test")
  
  expect_null(req$id)
  expect_equal(req$method, "notifications/test")
})

test_that("jsonrpc_response creates valid response", {
  resp <- jsonrpc_response(list(data = "test"), id = 5L)
  
  expect_equal(resp$jsonrpc, "2.0")
  expect_equal(resp$result$data, "test")
  expect_equal(resp$id, 5L)
  expect_null(resp$error)
})

test_that("jsonrpc_error creates valid error response", {
  err <- jsonrpc_error(-32600, "Invalid Request", id = 3L)
  
  expect_equal(err$jsonrpc, "2.0")
  expect_equal(err$error$code, -32600)
  expect_equal(err$error$message, "Invalid Request")
  expect_equal(err$id, 3L)
  expect_null(err$result)
})

test_that("jsonrpc_error with data includes data", {
  err <- jsonrpc_error(-32600, "Invalid Request", id = 3L, data = list(detail = "extra"))
  
  expect_equal(err$error$data$detail, "extra")
})

test_that("mcp_serialize produces valid JSON", {
  req <- jsonrpc_request("test", list(x = 1), id = 1L)
  json <- mcp_serialize(req)
  
  expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$jsonrpc, "2.0")
})

test_that("mcp_deserialize parses valid JSON", {
  json <- '{"jsonrpc":"2.0","method":"test","id":1}'
  msg <- mcp_deserialize(json)
  
  expect_equal(msg$jsonrpc, "2.0")
  expect_equal(msg$method, "test")
  expect_equal(msg$id, 1)
})

test_that("mcp_deserialize returns NULL for invalid JSON", {
  msg <- mcp_deserialize("not valid json {{{")
  expect_null(msg)
})

test_that("mcp_initialize_request creates proper format", {
  req <- mcp_initialize_request(
    client_info = list(name = "test-client", version = "1.0"),
    id = 1L
  )
  
  expect_equal(req$method, "initialize")
  expect_equal(req$params$protocolVersion, "2024-11-05")
  expect_equal(req$params$clientInfo$name, "test-client")
  expect_equal(req$id, 1L)
})

test_that("mcp_tools_list_request creates proper format", {
  req <- mcp_tools_list_request(id = 2L)
  
  expect_equal(req$method, "tools/list")
  expect_equal(req$id, 2L)
})

test_that("mcp_tools_call_request creates proper format", {
  req <- mcp_tools_call_request("my_tool", list(arg1 = "value"), id = 3L)
  
  expect_equal(req$method, "tools/call")
  expect_equal(req$params$name, "my_tool")
  expect_equal(req$params$arguments$arg1, "value")
  expect_equal(req$id, 3L)
})

test_that("mcp_resources_list_request creates proper format", {
  req <- mcp_resources_list_request(id = 4L)
  
  expect_equal(req$method, "resources/list")
  expect_equal(req$id, 4L)
})

test_that("mcp_resources_read_request creates proper format", {
  req <- mcp_resources_read_request("file:///test.txt", id = 5L)
  
  expect_equal(req$method, "resources/read")
  expect_equal(req$params$uri, "file:///test.txt")
  expect_equal(req$id, 5L)
})
