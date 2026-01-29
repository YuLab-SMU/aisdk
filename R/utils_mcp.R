#' MCP Utility Functions
#'
#' JSON-RPC 2.0 message helpers for MCP communication.
#'
#' @name utils_mcp
#' @keywords internal
NULL

# --- JSON-RPC 2.0 Message Creation ---

#' Create a JSON-RPC 2.0 request object
#' @param method The method name
#' @param params The parameters (list or NULL)
#' @param id The request ID (integer or string)
#' @return A list representing the JSON-RPC request
#' @keywords internal
jsonrpc_request <- function(method, params = NULL, id = NULL) {
  req <- list(
    jsonrpc = "2.0",
    method = method
  )
  if (!is.null(params)) {
    req$params <- params
  }
  if (!is.null(id)) {
    req$id <- id
  }
  req
}

#' Create a JSON-RPC 2.0 success response object
#' @param result The result value
#' @param id The request ID this is responding to
#' @return A list representing the JSON-RPC response
#' @keywords internal
jsonrpc_response <- function(result, id) {
  list(
    jsonrpc = "2.0",
    result = result,
    id = id
  )
}

#' Create a JSON-RPC 2.0 error response object
#' @param code The error code (integer)
#' @param message The error message
#' @param id The request ID this is responding to (can be NULL)
#' @param data Optional additional error data
#' @return A list representing the JSON-RPC error response
#' @keywords internal
jsonrpc_error <- function(code, message, id = NULL, data = NULL) {
  error_obj <- list(
    code = code,
    message = message
  )
  if (!is.null(data)) {
    error_obj$data <- data
  }
  resp <- list(
    jsonrpc = "2.0",
    error = error_obj,
    id = id
  )
  resp
}

# --- Standard JSON-RPC Error Codes ---

#' @keywords internal
JSONRPC_PARSE_ERROR <- -32700L
#' @keywords internal
JSONRPC_INVALID_REQUEST <- -32600L
#' @keywords internal
JSONRPC_METHOD_NOT_FOUND <- -32601L
#' @keywords internal
JSONRPC_INVALID_PARAMS <- -32602L
#' @keywords internal
JSONRPC_INTERNAL_ERROR <- -32603L

# --- MCP Protocol Helpers ---

#' Serialize a JSON-RPC message for MCP transport
#' @param msg The message list to serialize
#' @return A JSON string
#' @keywords internal
mcp_serialize <- function(msg) {
  jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null")
}

#' Deserialize a JSON-RPC message from MCP transport
#' @param json_str The JSON string to parse
#' @return A list, or NULL on parse error
#' @keywords internal
mcp_deserialize <- function(json_str) {
  tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

#' Create MCP initialize request
#' @param client_info List with name and version
#' @param capabilities Client capabilities
#' @param id Request ID
#' @return A JSON-RPC request for initialize
#' @keywords internal
mcp_initialize_request <- function(client_info, capabilities = structure(list(), names = character(0)), id = 1L) {
  jsonrpc_request(
    method = "initialize",
    params = list(
      protocolVersion = "2024-11-05",
      clientInfo = client_info,
      capabilities = capabilities
    ),
    id = id
  )
}

#' Create MCP initialized notification
#' @return A JSON-RPC notification
#' @keywords internal
mcp_initialized_notification <- function() {
  jsonrpc_request(method = "notifications/initialized")
}

#' Create MCP tools/list request
#' @param id Request ID
#' @return A JSON-RPC request
#' @keywords internal
mcp_tools_list_request <- function(id) {
  jsonrpc_request(method = "tools/list", id = id)
}

#' Create MCP tools/call request
#' @param tool_name The tool name
#' @param arguments Tool arguments as a list
#' @param id Request ID
#' @return A JSON-RPC request
#' @keywords internal
mcp_tools_call_request <- function(tool_name, arguments = structure(list(), names = character(0)), id) {
  jsonrpc_request(
    method = "tools/call",
    params = list(
      name = tool_name,
      arguments = arguments
    ),
    id = id
  )
}

#' Create MCP resources/list request
#' @param id Request ID
#' @return A JSON-RPC request
#' @keywords internal
mcp_resources_list_request <- function(id) {
  jsonrpc_request(method = "resources/list", id = id)
}

#' Create MCP resources/read request
#' @param uri The resource URI
#' @param id Request ID
#' @return A JSON-RPC request
#' @keywords internal
mcp_resources_read_request <- function(uri, id) {
  jsonrpc_request(
    method = "resources/read",
    params = list(uri = uri),
    id = id
  )
}
