
#' @title MCP SSE Client
#' @description
#' Connect to an MCP server via Server-Sent Events (SSE).
#' @name McpSseClient
#' @export
NULL

#' MCP SSE Client R6 Class
#'
#' Manages connection to a remote MCP server via SSE transport.
#'
#' @export
McpSseClient <- R6::R6Class(
  "McpSseClient",
  inherit = McpClient,
  public = list(
    #' @field endpoint The POST endpoint for sending messages (received from SSE init)
    endpoint = NULL,
    #' @field auth_headers Authentication headers
    auth_headers = NULL,

    #' @description
    #' Create a new MCP SSE Client
    #' @param url The SSE endpoint URL
    #' @param headers named list of headers (e.g. for auth)
    #' @return A new McpSseClient object
    initialize = function(url, headers = list()) {
      self$auth_headers <- headers
      
      # Background script to read SSE and print standardized messages
      bg_func <- function(url, headers) {
        # Ensure dependencies are loaded
        if (!requireNamespace("httr2", quietly = TRUE)) {
          stop("httr2 package is required for MCP SSE client")
        }
        
        tryCatch({
          req <- httr2::request(url)
          # Add standard SSE headers
          req <- httr2::req_headers(req, 
            "Accept" = "text/event-stream",
            "Cache-Control" = "no-cache",
            "Connection" = "keep-alive"
          )
          if (length(headers) > 0) req <- httr2::req_headers(req, !!!headers)
          
          # Connect
          resp <- httr2::req_perform_connection(req)
          on.exit(close(resp$body))
          
          reader <- resp$body
          buffer <- ""
          
          current_event <- NULL
          
          while (TRUE) {
            chunk_raw <- reader$read(4096)
            if (length(chunk_raw) == 0) {
              if (reader$is_complete) break
              Sys.sleep(0.01)
              next
            }
            
            chunk <- rawToChar(chunk_raw)
            # DEBUG: Log raw chunk (truncated)
            # message("DEBUG_CHUNK: ", substr(chunk, 1, 50))
            buffer <- paste0(buffer, chunk)
            
            while (grepl("\n", buffer)) {
              lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
              
              # Handle trailing partial line
              if (!endsWith(buffer, "\n")) {
                buffer <- tail(lines, 1)
                lines <- head(lines, -1)
              } else {
                buffer <- ""
              }
              
              if (length(lines) == 0) next
              
              for (line in lines) {
                line <- gsub("\r$", "", line)
                # DEBUG: Log every line
                message("DEBUG_LINE: ", line)
                
                if (!nzchar(line)) {
                  # Empty line = dispatch event
                  current_event <- NULL
                  next
                }
                
                if (startsWith(line, "event: ")) {
                  current_event <- substring(line, 8)
                } else if (startsWith(line, "data: ")) {
                  data <- substring(line, 7)
                  
                  if (data == "[DONE]") {
                    quit(save = "no")
                  }
                  
                  # Check current event type
                  if (!is.null(current_event) && current_event == "endpoint") {
                     cat("ENDPOINT:", data, "\n", sep = "")
                  } else {
                     # Regular message
                     cat(data, "\n") 
                  }
                  utils::flush.console()
                }
              }
            }
          }
        }, error = function(e) {
          # print to stderr so it can be captured by parent
          message("Error in MCP SSE background process: ", conditionMessage(e))
          quit(save = "no", status = 1)
        })
      }
      
      self$process <- callr::r_bg(
        func = bg_func,
        args = list(url = url, headers = headers),
        supervise = TRUE,
        stdout = "|",
        stderr = "|" 
      )
      
      private$perform_sse_handshake(url)
      
      invisible(self)
    }
  ),
  
  private = list(
    perform_sse_handshake = function(base_url) {
      start_time <- Sys.time()
      found_endpoint <- FALSE
      
      while (!found_endpoint) {
        if (!self$is_alive()) {
          # Capture stderr to explain why it died
          err_lines <- self$process$read_error_lines()
          err_msg <- if (length(err_lines) > 0) paste(err_lines, collapse = "\n") else "Unknown error"
          stop("SSE connection process died shortly after start. Error:\n", err_msg)
        }
        if (difftime(Sys.time(), start_time, units="secs") > 15) {
           # Capture debug logs
           err_lines <- self$process$read_error_lines()
           err_msg <- if (length(err_lines) > 0) paste(err_lines, collapse = "\n") else "No debug output"
           stop("Timeout waiting for SSE endpoint. Debug logs:\n", err_msg)
        }
        
        lines <- self$process$read_output_lines()
        for (line in lines) {
           if (startsWith(line, "ENDPOINT:")) {
             endpoint_path <- substring(line, 10)
             
             # Resolve endpoint logic (same as before)
             if (grepl("^https?://", endpoint_path)) {
               self$endpoint <- endpoint_path
             } else {
                base_parsed <- httr2::url_parse(base_url)
                p <- base_parsed
                if (startsWith(endpoint_path, "/")) {
                   p$path <- endpoint_path
                } else {
                   p$path <- paste0(sub("/$", "", p$path %||% ""), "/", endpoint_path)
                }
                self$endpoint <- httr2::url_build(p)
             }
             found_endpoint <- TRUE
             break
           }
        }
        Sys.sleep(0.1)
      }
      
      # Handshake
      init_req <- mcp_initialize_request(
         client_info = list(name = "r-ai-sdk-sse", version = "0.7.0"),
         capabilities = structure(list(), names = character(0)),
         id = private$next_id()
      )
      resp <- private$send_request(init_req)
      if (!is.null(resp$error)) stop("MCP initialization failed: ", resp$error$message)
       
      self$server_info <- resp$result$serverInfo
      self$capabilities <- resp$result$capabilities
       
      notif <- mcp_initialized_notification()
      private$send_notification(notif)
    },
    
    send_request = function(request) {
      if (is.null(self$endpoint)) stop("MCP SSE Endpoint not initialized")
      
      req <- httr2::request(self$endpoint)
      req <- httr2::req_headers(req, !!!self$auth_headers, "Content-Type" = "application/json")
      req <- httr2::req_body_json(req, request)
      
      httr2::req_perform(req)
      
      target_id <- request$id
      
      start_wait <- Sys.time()
      while (TRUE) {
        if (!self$is_alive()) {
             err_lines <- self$process$read_error_lines()
             err_msg <- if (length(err_lines) > 0) paste(err_lines, collapse = "\n") else "Unknown error"
             stop("SSE connection process died while waiting for response. Error:\n", err_msg)
        }
        if (difftime(Sys.time(), start_wait, units="secs") > 60) stop("Timeout waiting for MCP response")
        
        lines <- self$process$read_output_lines()
        for (line in lines) {
           if (startsWith(line, "{")) {
             msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector=FALSE), error=function(e) NULL)
             if (!is.null(msg)) {
               if (!is.null(msg$id) && msg$id == target_id) {
                 return(msg)
               }
             }
           }
        }
        Sys.sleep(0.01)
      }
    },
    
    send_notification = function(notification) {
      req <- httr2::request(self$endpoint)
      req <- httr2::req_headers(req, !!!self$auth_headers, "Content-Type" = "application/json")
      req <- httr2::req_body_json(req, notification)
      httr2::req_perform(req)
    }
  )
)

#' Create MCP SSE Client
#' @param url The SSE endpoint URL
#' @param headers named list of headers (e.g. for auth)
#' @export 
create_mcp_sse_client <- function(url, headers = list()) {
  McpSseClient$new(url, headers)
}
