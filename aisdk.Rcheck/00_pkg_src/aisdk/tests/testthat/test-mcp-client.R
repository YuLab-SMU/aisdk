# Test MCP Client
# Note: Most client tests require mocking processx, which is complex.
# These tests focus on the class structure and helper methods.

test_that("McpClient class exists", {
  expect_true(R6::is.R6Class(McpClient))
})

test_that("create_mcp_client function exists", {
  expect_true(is.function(create_mcp_client))
})

# Integration tests would require a real MCP server or extensive mocking
# These are documented as examples in the demo script
