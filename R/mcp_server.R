#' MCP Server
#'
#' Expose R functions as MCP tools to external clients.
#'
#' @name McpServer
#' @export
NULL

#' MCP Server R6 Class
#'
#' Serves R tools and resources via MCP protocol over stdio.
#'
#' @export
McpServer <- R6::R6Class(
  "McpServer",
  public = list(
    #' @field name Server name
    name = NULL,

    #' @field version Server version
    version = NULL,

    #' @field tools Registered tools
    tools = NULL,

    #' @field resources Registered resources
    resources = NULL,

    #' @description
    #' Create a new MCP Server
    #' @param name Server name
    #' @param version Server version
    #' @return A new McpServer object
    initialize = function(name = "r-mcp-server", version = "0.1.0") {
      self$name <- name
      self$version <- version
      self$tools <- list()
      self$resources <- list()
      invisible(self)
    },

    #' @description
    #' Add a tool to the server
    #' @param tool A Tool object from the SDK
    #' @return self (for chaining)
    add_tool = function(tool) {
      if (!inherits(tool, "Tool")) {
        stop("tool must be a Tool object")
      }
      self$tools[[tool$name]] <- tool
      invisible(self)
    },

    #' @description
    #' Add a resource to the server
    #' @param uri Resource URI
    #' @param name Resource name
    #' @param description Resource description
    #' @param mime_type MIME type
    #' @param read_fn Function that returns the resource content
    #' @return self (for chaining)
    add_resource = function(uri, name, description = "", mime_type = "text/plain", read_fn) {
      self$resources[[uri]] <- list(
        uri = uri,
        name = name,
        description = description,
        mimeType = mime_type,
        read_fn = read_fn
      )
      invisible(self)
    },

    #' @description
    #' Start listening for MCP requests on stdin/stdout
    #' This is a blocking call.
    listen = function() {
      message("MCP Server '", self$name, "' listening on stdio...")

      while (TRUE) {
        # Read a line from stdin
        line <- readLines(con = stdin(), n = 1, warn = FALSE)

        if (length(line) == 0) {
          # EOF - client disconnected
          break
        }

        if (!nzchar(trimws(line))) {
          next
        }

        # Process the request
        response <- private$handle_message(line)

        # Send response if not NULL (notifications don't get responses)
        if (!is.null(response)) {
          private$send_response(response)
        }
      }

      message("MCP Server stopped.")
    },

    #' @description
    #' Process a single MCP message (for testing)
    #' @param json_str The JSON-RPC message
    #' @return The response, or NULL for notifications
    process_message = function(json_str) {
      private$handle_message(json_str)
    }
  ),

  private = list(
    initialized = FALSE,

    handle_message = function(json_str) {
      # Parse the message
      msg <- mcp_deserialize(json_str)

      if (is.null(msg)) {
        return(jsonrpc_error(JSONRPC_PARSE_ERROR, "Parse error", id = NULL))
      }

      # Check for valid JSON-RPC
      if (is.null(msg$method)) {
        return(jsonrpc_error(JSONRPC_INVALID_REQUEST, "Invalid Request", id = msg$id))
      }

      # Route to handler
      handler <- private$get_handler(msg$method)

      if (is.null(handler)) {
        # Check if this is a notification (no id)
        if (is.null(msg$id)) {
          return(NULL)
        }
        return(jsonrpc_error(JSONRPC_METHOD_NOT_FOUND, 
                            paste("Method not found:", msg$method), 
                            id = msg$id))
      }

      # Execute handler
      tryCatch({
        result <- handler(msg$params)
        # Notifications don't get responses
        if (is.null(msg$id)) {
          return(NULL)
        }
        jsonrpc_response(result, id = msg$id)
      }, error = function(e) {
        jsonrpc_error(JSONRPC_INTERNAL_ERROR, conditionMessage(e), id = msg$id)
      })
    },

    get_handler = function(method) {
      handlers <- list(
        "initialize" = private$handle_initialize,
        "notifications/initialized" = private$handle_initialized,
        "tools/list" = private$handle_tools_list,
        "tools/call" = private$handle_tools_call,
        "resources/list" = private$handle_resources_list,
        "resources/read" = private$handle_resources_read
      )
      handlers[[method]]
    },

    handle_initialize = function(params) {
      list(
        protocolVersion = "2024-11-05",
        serverInfo = list(
          name = self$name,
          version = self$version
        ),
        capabilities = list(
          tools = if (length(self$tools) > 0) list() else NULL,
          resources = if (length(self$resources) > 0) list() else NULL
        )
      )
    },

    handle_initialized = function(params) {
      private$initialized <- TRUE
      NULL
    },

    handle_tools_list = function(params) {
      tool_defs <- lapply(self$tools, function(t) {
        list(
          name = t$name,
          description = t$description,
          inputSchema = t$parameters
        )
      })
      list(tools = unname(tool_defs))
    },

    handle_tools_call = function(params) {
      tool_name <- params$name
      arguments <- params$arguments %||% list()

      tool <- self$tools[[tool_name]]
      if (is.null(tool)) {
        stop("Unknown tool: ", tool_name)
      }

      # Execute the tool
      result <- tryCatch({
        tool$run(arguments)
      }, error = function(e) {
        return(list(
          content = list(
            list(type = "text", text = paste("Error:", conditionMessage(e)))
          ),
          isError = TRUE
        ))
      })

      # Format result as MCP content
      if (is.list(result) && !is.null(result$content)) {
        result
      } else {
        list(
          content = list(
            list(type = "text", text = as.character(result))
          )
        )
      }
    },

    handle_resources_list = function(params) {
      resource_defs <- lapply(self$resources, function(r) {
        list(
          uri = r$uri,
          name = r$name,
          description = r$description,
          mimeType = r$mimeType
        )
      })
      list(resources = unname(resource_defs))
    },

    handle_resources_read = function(params) {
      uri <- params$uri
      resource <- self$resources[[uri]]

      if (is.null(resource)) {
        stop("Unknown resource: ", uri)
      }

      content <- resource$read_fn()

      list(
        contents = list(
          list(
            uri = uri,
            mimeType = resource$mimeType,
            text = as.character(content)
          )
        )
      )
    },

    send_response = function(response) {
      json_str <- mcp_serialize(response)
      cat(json_str, "\n", sep = "")
      flush(stdout())
    }
  )
)

#' Create an MCP Server
#'
#' Convenience function to create an MCP server.
#'
#' @param name Server name
#' @param version Server version
#' @return An McpServer object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a server with a custom tool
#' server <- create_mcp_server("my-r-server")
#'
#' # Add a tool
#' server$add_tool(tool(
#'   name = "calculate",
#'   description = "Perform a calculation",
#'   parameters = z_object(
#'     expression = z_string(description = "R expression to evaluate")
#'   ),
#'   execute = function(args) {
#'     eval(parse(text = args$expression))
#'   }
#' ))
#'
#' # Start listening (blocking)
#' server$listen()
#' }
create_mcp_server <- function(name = "r-mcp-server", version = "0.1.0") {
  McpServer$new(name, version)
}
