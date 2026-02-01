#' MCP Client
#'
#' Connect to and communicate with an MCP server process.
#'
#' @name McpClient
#' @export
NULL

#' MCP Client R6 Class
#'
#' Manages connection to an external MCP server via stdio.
#'
#' @export
McpClient <- R6::R6Class(
  "McpClient",
  public = list(
    #' @field process The processx process object
    process = NULL,

    #' @field server_info Information about the connected server
    server_info = NULL,

    #' @field capabilities Server capabilities
    capabilities = NULL,

    #' @description

    #' Create a new MCP Client
    #' @param command The command to run (e.g., "npx", "python")
    #' @param args Command arguments (e.g., c("-y", "@modelcontextprotocol/server-github"))
    #' @param env Environment variables as a named character vector
    #' @return A new McpClient object
    initialize = function(command, args = character(), env = NULL) {
      # Build environment
      proc_env <- if (!is.null(env)) {
        c(Sys.getenv(), env)
      } else {
        NULL
      }

      # Start the MCP server process
      self$process <- processx::process$new(
        command = command,
        args = args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        env = proc_env
      )

      # Perform MCP handshake
      private$perform_handshake()

      invisible(self)
    },

    #' @description
    #' List available tools from the MCP server
    #' @return A list of tool definitions
    list_tools = function() {
      private$ensure_alive()
      req <- mcp_tools_list_request(id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result$tools %||% list()
    },

    #' @description
    #' Call a tool on the MCP server
    #' @param name The tool name
    #' @param arguments Tool arguments as a named list
    #' @return The tool result
    call_tool = function(name, arguments = list()) {
      private$ensure_alive()
      req <- mcp_tools_call_request(name, arguments, id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP tool error: ", resp$error$message)
      }

      resp$result
    },

    #' @description
    #' List available resources from the MCP server
    #' @return A list of resource definitions
    list_resources = function() {
      private$ensure_alive()
      req <- mcp_resources_list_request(id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result$resources %||% list()
    },

    #' @description
    #' Read a resource from the MCP server
    #' @param uri The resource URI
    #' @return The resource contents
    read_resource = function(uri) {
      private$ensure_alive()
      req <- mcp_resources_read_request(uri, id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result
    },

    #' @description
    #' Check if the MCP server process is alive
    #' @return TRUE if alive, FALSE otherwise
    is_alive = function() {
      !is.null(self$process) && self$process$is_alive()
    },

    #' @description
    #' Close the MCP client connection
    close = function() {
      if (!is.null(self$process) && self$process$is_alive()) {
        self$process$kill()
      }
      invisible(self)
    },

    #' @description
    #' Convert MCP tools to SDK Tool objects
    #' @return A list of Tool objects
    as_sdk_tools = function() {
      mcp_tools <- self$list_tools()
      lapply(mcp_tools, function(t) {
        private$mcp_tool_to_sdk_tool(t)
      })
    }
  ),

  private = list(
    request_id = 0L,

    next_id = function() {
      private$request_id <- private$request_id + 1L
      private$request_id
    },

    ensure_alive = function() {
      if (!self$is_alive()) {
        stop("MCP server process is not running")
      }
    },

    perform_handshake = function() {
      # Send initialize request
      init_req <- mcp_initialize_request(
        client_info = list(name = "r-ai-sdk", version = "0.7.0"),
        capabilities = structure(list(), names = character(0)),
        id = private$next_id()
      )

      resp <- private$send_request(init_req)

      if (!is.null(resp$error)) {
        stop("MCP initialization failed: ", resp$error$message)
      }

      self$server_info <- resp$result$serverInfo
      self$capabilities <- resp$result$capabilities

      # Send initialized notification
      notif <- mcp_initialized_notification()
      private$send_notification(notif)

      invisible(self)
    },

    send_request = function(request) {
      json_str <- paste0(mcp_serialize(request), "\n")
      self$process$write_input(json_str)

      # Read response (blocking with timeout)
      response_str <- private$read_response()
      mcp_deserialize(response_str)
    },

    send_notification = function(notification) {
      json_str <- paste0(mcp_serialize(notification), "\n")
      self$process$write_input(json_str)
    },

    read_response = function(timeout_ms = 30000) {
      # Read line from stdout
      start_time <- Sys.time()
      result <- ""

      while (TRUE) {
        # Check timeout
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
        if (elapsed > timeout_ms) {
          stop("MCP response timeout")
        }

        # Try to read
        chunk <- self$process$read_output_lines(n = 1)
        if (length(chunk) > 0 && nzchar(chunk)) {
          return(chunk)
        }

        # Small sleep to avoid busy waiting

        Sys.sleep(0.01)
      }
    },

    mcp_tool_to_sdk_tool = function(mcp_tool) {
      # Create a closure that calls back to this client
      client <- self
      tool_name <- mcp_tool$name

      tool(
        name = mcp_tool$name,
        description = mcp_tool$description %||% "",
        parameters = private$schema_from_mcp(mcp_tool$inputSchema),
        execute = function(args) {
          result <- client$call_tool(tool_name, args)
          # Extract text content if present
          if (!is.null(result$content)) {
            texts <- sapply(result$content, function(c) {
              if (c$type == "text") c$text else ""
            })
            paste(texts, collapse = "\n")
          } else {
            jsonlite::toJSON(result, auto_unbox = TRUE)
          }
        }
      )
    },

    schema_from_mcp = function(input_schema) {
      # Convert MCP input schema to SDK schema
      # MCP uses standard JSON Schema format
      if (is.null(input_schema)) {
        return(z_object())
      }

      # For now, pass through as raw schema
      # The SDK tool system will handle it
      structure(
        input_schema,
        class = c("z_schema", "z_object")
      )
    }
  )
)

#' Create an MCP Client
#'
#' Convenience function to create and connect to an MCP server.
#'
#' @param command The command to run the MCP server
#' @param args Command arguments
#' @param env Environment variables
#' @return An McpClient object
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to GitHub MCP server
#' client <- create_mcp_client(
#'   "npx",
#'   c("-y", "@modelcontextprotocol/server-github"),
#'   env = c(GITHUB_PERSONAL_ACCESS_TOKEN = Sys.getenv("GITHUB_TOKEN"))
#' )
#'
#' # List available tools
#' tools <- client$list_tools()
#'
#' # Use tools with generate_text
#' result <- generate_text(
#'   model = "openai:gpt-4o",
#'   prompt = "List my GitHub repos",
#'   tools = client$as_sdk_tools()
#' )
#'
#' client$close()
#' }
create_mcp_client <- function(command, args = character(), env = NULL) {
  McpClient$new(command, args, env)
}
