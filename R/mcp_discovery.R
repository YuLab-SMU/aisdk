#' @title Distributed MCP Ecosystem
#' @description
#' Service discovery and dynamic composition for MCP (Model Context Protocol)
#' servers. Implements mDNS/DNS-SD discovery, skill negotiation, and
#' hot-swapping of tools at runtime.
#' @name mcp_discover
NULL

#' @title MCP Discovery Class
#' @description
#' R6 class for discovering MCP servers on the local network using
#' mDNS/DNS-SD (Bonjour) protocol.
#' @export
McpDiscovery <- R6::R6Class(

  "McpDiscovery",

  public = list(
    #' @field discovered List of discovered MCP endpoints.
    discovered = NULL,

    #' @field registry_url URL of the remote skill registry.
    registry_url = NULL,

    #' @description
    #' Create a new MCP Discovery instance.
    #' @param registry_url Optional URL for remote skill registry.
    #' @return A new McpDiscovery object.
    initialize = function(registry_url = NULL) {
      self$discovered <- list()
      self$registry_url <- registry_url %||% getOption("aisdk.skill_registry", "https://skills.r-ai.dev")
      invisible(self)
    },

    #' @description
    #' Scan the local network for MCP servers.
    #' @param timeout_seconds How long to scan for services.
    #' @param service_type The mDNS service type to look for.
    #' @return A data frame of discovered services.
    scan_network = function(timeout_seconds = 5, service_type = "_mcp._tcp") {
      # Check for dns-sd or avahi-browse availability
      dns_sd <- Sys.which("dns-sd")
      avahi <- Sys.which("avahi-browse")

      if (nchar(dns_sd) > 0) {
        services <- private$scan_with_dns_sd(service_type, timeout_seconds)
      } else if (nchar(avahi) > 0) {
        services <- private$scan_with_avahi(service_type, timeout_seconds)
      } else {
        # Fallback: scan common ports on localhost
        services <- private$scan_localhost_ports()
      }

      # Update discovered list
      for (i in seq_len(nrow(services))) {
        key <- paste0(services$host[i], ":", services$port[i])
        self$discovered[[key]] <- as.list(services[i, ])
      }

      services
    },

    #' @description
    #' Register a known MCP endpoint manually.
    #' @param name Service name.
    #' @param host Hostname or IP address.
    #' @param port Port number.
    #' @param capabilities Optional list of capabilities.
    #' @return Self (invisibly).
    register = function(name, host, port, capabilities = NULL) {
      key <- paste0(host, ":", port)
      self$discovered[[key]] <- list(
        name = name,
        host = host,
        port = port,
        capabilities = capabilities,
        registered_at = Sys.time()
      )
      invisible(self)
    },

    #' @description
    #' Query a discovered server for its capabilities.
    #' @param host Hostname or IP.
    #' @param port Port number.
    #' @return A list of server capabilities.
    query_capabilities = function(host, port) {
      # Connect via SSE or stdio based on server type
      url <- sprintf("http://%s:%d", host, port)

      tryCatch({
        # Try SSE endpoint first
        req <- httr2::request(url)
        req <- httr2::req_url_path_append(req, "capabilities")
        req <- httr2::req_timeout(req, 5)
        response <- httr2::req_perform(req)

        httr2::resp_body_json(response)
      }, error = function(e) {
        # Return empty capabilities on error
        list(tools = list(), resources = list(), prompts = list())
      })
    },

    #' @description
    #' List all discovered MCP endpoints.
    #' @return A data frame of endpoints.
    list_endpoints = function() {
      if (length(self$discovered) == 0) {
        return(data.frame(
          name = character(),
          host = character(),
          port = integer(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(self$discovered, function(x) {
        data.frame(
          name = x$name %||% "unknown",
          host = x$host,
          port = x$port,
          stringsAsFactors = FALSE
        )
      }))
    },

    #' @description
    #' Search the remote registry for skills.
    #' @param query Search query.
    #' @return A data frame of matching skills.
    search_registry = function(query) {
      tryCatch({
        req <- httr2::request(self$registry_url)
        req <- httr2::req_url_path_append(req, "api", "search")
        req <- httr2::req_url_query(req, q = query)
        req <- httr2::req_timeout(req, 10)
        response <- httr2::req_perform(req)

        httr2::resp_body_json(response)
      }, error = function(e) {
        message("Registry search failed: ", conditionMessage(e))
        list()
      })
    },

    #' @description
    #' Print method for McpDiscovery.
    print = function() {
      cat("<McpDiscovery>\n")
      cat("  Discovered endpoints:", length(self$discovered), "\n")
      cat("  Registry URL:", self$registry_url, "\n")
      invisible(self)
    }
  ),

  private = list(
    scan_with_dns_sd = function(service_type, timeout) {
      # Use macOS dns-sd command
      result <- tryCatch({
        processx::run(
          "dns-sd",
          c("-B", service_type, "local"),
          timeout = timeout,
          error_on_status = FALSE
        )
      }, error = function(e) {
        list(stdout = "")
      })

      private$parse_dns_sd_output(result$stdout)
    },

    scan_with_avahi = function(service_type, timeout) {
      # Use Linux avahi-browse command
      result <- tryCatch({
        processx::run(
          "avahi-browse",
          c("-t", "-r", service_type),
          timeout = timeout,
          error_on_status = FALSE
        )
      }, error = function(e) {
        list(stdout = "")
      })

      private$parse_avahi_output(result$stdout)
    },

    scan_localhost_ports = function() {
      # Scan common MCP ports on localhost
      common_ports <- c(3000, 3001, 8000, 8080, 9000)
      found <- list()

      for (port in common_ports) {
        if (private$check_port("127.0.0.1", port)) {
          found[[length(found) + 1]] <- list(
            name = sprintf("localhost:%d", port),
            host = "127.0.0.1",
            port = port
          )
        }
      }

      if (length(found) == 0) {
        return(data.frame(
          name = character(),
          host = character(),
          port = integer(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(found, as.data.frame))
    },

    check_port = function(host, port) {
      tryCatch({
        con <- socketConnection(host, port, open = "r", blocking = TRUE, timeout = 1)
        close(con)
        TRUE
      }, error = function(e) {
        FALSE
      })
    },

    parse_dns_sd_output = function(output) {
      # Parse dns-sd browse output
      lines <- strsplit(output, "\n")[[1]]
      services <- list()

      for (line in lines) {
        if (grepl("Add", line)) {
          parts <- strsplit(trimws(line), "\\s+")[[1]]
          if (length(parts) >= 7) {
            services[[length(services) + 1]] <- list(
              name = parts[7],
              host = "localhost",  # Would need resolve step
              port = 0
            )
          }
        }
      }

      if (length(services) == 0) {
        return(data.frame(
          name = character(),
          host = character(),
          port = integer(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(services, as.data.frame))
    },

    parse_avahi_output = function(output) {
      # Parse avahi-browse output
      lines <- strsplit(output, "\n")[[1]]
      services <- list()
      current <- list()

      for (line in lines) {
        if (grepl("^=", line)) {
          if (length(current) > 0) {
            services[[length(services) + 1]] <- current
          }
          current <- list(name = "", host = "", port = 0)
        } else if (grepl("hostname", line, ignore.case = TRUE)) {
          current$host <- sub(".*\\[(.*)\\].*", "\\1", line)
        } else if (grepl("port", line, ignore.case = TRUE)) {
          current$port <- as.integer(sub(".*\\[(.*)\\].*", "\\1", line))
        }
      }

      if (length(current) > 0 && !is.null(current$host)) {
        services[[length(services) + 1]] <- current
      }

      if (length(services) == 0) {
        return(data.frame(
          name = character(),
          host = character(),
          port = integer(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(services, as.data.frame))
    }
  )
)

#' @title MCP Router Class
#' @description
#' A virtual MCP server that aggregates tools from multiple downstream
#' MCP servers into a unified interface. Supports hot-swapping and
#' skill negotiation.
#' @export
McpRouter <- R6::R6Class(
  "McpRouter",

  public = list(
    #' @field clients List of connected MCP clients.
    clients = NULL,

    #' @field tool_map Mapping of tool names to their source clients.
    tool_map = NULL,

    #' @field capabilities Aggregated capabilities from all clients.
    capabilities = NULL,

    #' @description
    #' Create a new MCP Router.
    #' @return A new McpRouter object.
    initialize = function() {
      self$clients <- list()
      self$tool_map <- list()
      self$capabilities <- list(tools = list(), resources = list())
      invisible(self)
    },

    #' @description
    #' Add an MCP client to the router.
    #' @param name Unique name for this client.
    #' @param client An McpClient object.
    #' @return Self (invisibly).
    add_client = function(name, client) {
      if (!inherits(client, "McpClient")) {
        rlang::abort("client must be an McpClient object")
      }

      self$clients[[name]] <- client

      # Refresh tool mappings
      private$refresh_tools()

      invisible(self)
    },

    #' @description
    #' Connect to an MCP server and add it to the router.
    #' @param name Unique name for this connection.
    #' @param command Command to run the MCP server.
    #' @param args Command arguments.
    #' @param env Environment variables.
    #' @return Self (invisibly).
    connect = function(name, command, args = character(), env = NULL) {
      client <- McpClient$new(command, args, env)
      self$add_client(name, client)
      invisible(self)
    },

    #' @description
    #' Remove an MCP client from the router (hot-swap out).
    #' @param name Name of the client to remove.
    #' @return Self (invisibly).
    remove_client = function(name) {
      if (!is.null(self$clients[[name]])) {
        # Close the client connection
        tryCatch(
          self$clients[[name]]$close(),
          error = function(e) NULL
        )
        self$clients[[name]] <- NULL

        # Refresh tool mappings
        private$refresh_tools()
      }

      invisible(self)
    },

    #' @description
    #' List all available tools across all connected clients.
    #' @return A list of tool definitions.
    list_tools = function() {
      self$capabilities$tools
    },

    #' @description
    #' Call a tool, routing to the appropriate client.
    #' @param name Tool name.
    #' @param arguments Tool arguments.
    #' @return The tool result.
    call_tool = function(name, arguments = list()) {
      if (is.null(self$tool_map[[name]])) {
        rlang::abort(paste0("Tool not found: ", name))
      }

      client_name <- self$tool_map[[name]]
      client <- self$clients[[client_name]]

      if (is.null(client) || !client$is_alive()) {
        rlang::abort(paste0("Client '", client_name, "' is not available"))
      }

      client$call_tool(name, arguments)
    },

    #' @description
    #' Get all tools as SDK Tool objects for use with generate_text.
    #' @return A list of Tool objects.
    as_sdk_tools = function() {
      router <- self

      lapply(names(self$tool_map), function(tool_name) {
        tool_def <- self$capabilities$tools[[tool_name]]

        tool(
          name = tool_name,
          description = tool_def$description %||% "",
          parameters = private$schema_from_mcp(tool_def$inputSchema),
          execute = function(args) {
            result <- router$call_tool(tool_name, args)
            # Extract text content
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
      })
    },

    #' @description
    #' Negotiate capabilities with a specific client.
    #' @param client_name Name of the client.
    #' @return A list of negotiated capabilities.
    negotiate = function(client_name) {
      client <- self$clients[[client_name]]
      if (is.null(client)) {
        return(NULL)
      }

      list(
        name = client_name,
        server_info = client$server_info,
        capabilities = client$capabilities,
        tools = client$list_tools()
      )
    },

    #' @description
    #' Get router status.
    #' @return A list with status information.
    status = function() {
      client_status <- lapply(names(self$clients), function(name) {
        client <- self$clients[[name]]
        list(
          name = name,
          alive = client$is_alive(),
          tools = length(client$list_tools())
        )
      })

      list(
        total_clients = length(self$clients),
        active_clients = sum(sapply(client_status, function(x) x$alive)),
        total_tools = length(self$tool_map),
        clients = client_status
      )
    },

    #' @description
    #' Close all client connections.
    close = function() {
      for (name in names(self$clients)) {
        tryCatch(
          self$clients[[name]]$close(),
          error = function(e) NULL
        )
      }
      self$clients <- list()
      self$tool_map <- list()
      self$capabilities <- list(tools = list(), resources = list())
      invisible(self)
    },

    #' @description
    #' Print method for McpRouter.
    print = function() {
      status <- self$status()
      cat("<McpRouter>\n")
      cat("  Clients:", status$total_clients, "(", status$active_clients, "active )\n")
      cat("  Tools:", status$total_tools, "\n")
      invisible(self)
    }
  ),

  private = list(
    refresh_tools = function() {
      self$tool_map <- list()
      self$capabilities$tools <- list()

      for (name in names(self$clients)) {
        client <- self$clients[[name]]
        if (!is.null(client) && client$is_alive()) {
          tryCatch({
            tools <- client$list_tools()
            for (tool in tools) {
              tool_name <- tool$name
              # Prefix with client name if there's a conflict
              if (!is.null(self$tool_map[[tool_name]])) {
                tool_name <- paste0(name, "_", tool_name)
              }
              self$tool_map[[tool_name]] <- name
              self$capabilities$tools[[tool_name]] <- tool
            }
          }, error = function(e) {
            message("Failed to get tools from ", name, ": ", conditionMessage(e))
          })
        }
      }
    },

    schema_from_mcp = function(input_schema) {
      if (is.null(input_schema)) {
        return(z_object())
      }
      structure(input_schema, class = c("z_schema", "z_object"))
    }
  )
)

#' @title Create MCP Discovery
#' @description
#' Factory function to create an MCP discovery instance.
#' @param registry_url Optional URL for remote skill registry.
#' @return An McpDiscovery object.
#' @export
#' @examples
#' \dontrun{
#' # Create discovery instance
#' discovery <- mcp_discover()
#'
#' # Scan local network
#' services <- discovery$scan_network()
#'
#' # Register a known endpoint
#' discovery$register("my-server", "localhost", 3000)
#'
#' # List all discovered endpoints
#' discovery$list_endpoints()
#' }
mcp_discover <- function(registry_url = NULL) {
  McpDiscovery$new(registry_url)
}

#' @title Create MCP Router
#' @description
#' Factory function to create an MCP router for aggregating multiple servers.
#' @return An McpRouter object.
#' @export
#' @examples
#' \dontrun{
#' # Create router
#' router <- mcp_router()
#'
#' # Connect to multiple MCP servers
#' router$connect("github", "npx", c("-y", "@modelcontextprotocol/server-github"))
#' router$connect("filesystem", "npx", c("-y", "@modelcontextprotocol/server-filesystem"))
#'
#' # Use aggregated tools with generate_text
#' result <- generate_text(
#'   model = "openai:gpt-4o",
#'   prompt = "List my GitHub repos and save to a file",
#'   tools = router$as_sdk_tools()
#' )
#'
#' # Hot-swap: remove a server
#' router$remove_client("github")
#'
#' # Cleanup
#' router$close()
#' }
mcp_router <- function() {
  McpRouter$new()
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
