# OpenAI built-in (model-hosted) tools for the Responses API.
#
# These factories build the plain `list(type = "...", ...)` payloads the
# Responses API expects under `body$tools`. They do not run anything locally —
# OpenAI's servers execute the tool when the model decides to call it. The
# results stream back through the same Response object.
#
# Pass the result of any of these to `generate_text(tools = list(...))`. They
# can be mixed freely with regular aisdk `Tool` objects; the OpenAI provider's
# tool dispatcher passes non-Tool list objects through verbatim.

#' OpenAI built-in web_search tool
#'
#' @description Build the configuration for OpenAI's model-hosted web search,
#'   used via the Responses API. The model decides whether to search based on
#'   the prompt.
#'
#' @param allowed_domains Optional character vector of domains the search is
#'   restricted to. Forwarded as `filters.allowed_domains`.
#' @param user_location Optional named list with any of `city`, `country`,
#'   `region`, `timezone` for localized results.
#' @param search_context_size Optional `"low"`, `"medium"` (API default), or
#'   `"high"` — controls how much retrieved context the model is given.
#' @param type The tool type identifier. Defaults to `"web_search"`; pass
#'   `"web_search_preview"` for the older preview integration.
#'
#' @return A plain list ready to drop into `tools = list(...)`.
#' @export
#' @examples
#' \dontrun{
#' model <- create_openai()$responses_model("gpt-5")
#' generate_text(
#'   model,
#'   "Latest material AAPL news?",
#'   tools = list(openai_web_search_tool(search_context_size = "high"))
#' )
#' }
openai_web_search_tool <- function(allowed_domains = NULL,
                                   user_location = NULL,
                                   search_context_size = NULL,
                                   type = c("web_search", "web_search_preview")) {
  type <- match.arg(type)
  tool <- list(type = type)
  if (!is.null(allowed_domains)) {
    tool$filters <- list(allowed_domains = as.list(allowed_domains))
  }
  if (!is.null(user_location)) {
    if (!is.list(user_location)) {
      rlang::abort("`user_location` must be a named list (city/country/region/timezone).")
    }
    tool$user_location <- user_location
  }
  if (!is.null(search_context_size)) {
    if (!search_context_size %in% c("low", "medium", "high")) {
      rlang::abort("`search_context_size` must be one of 'low', 'medium', 'high'.")
    }
    tool$search_context_size <- search_context_size
  }
  tool
}

#' OpenAI built-in file_search tool
#'
#' @description Configure the Responses API's hosted RAG over one or more
#'   vector stores you've uploaded to OpenAI.
#'
#' @param vector_store_ids Character vector of vector-store IDs to search.
#'   Required, non-empty.
#' @param max_num_results Optional integer cap on how many chunks the model
#'   sees per call.
#' @param ranking_options Optional named list for advanced ranking config
#'   (e.g. `list(ranker = "auto", score_threshold = 0.5)`).
#'
#' @return A plain list ready to drop into `tools = list(...)`.
#' @export
#' @examples
#' \dontrun{
#' generate_text(
#'   model,
#'   "What does the design doc say about caching?",
#'   tools = list(openai_file_search_tool(c("vs_abc123")))
#' )
#' }
openai_file_search_tool <- function(vector_store_ids,
                                    max_num_results = NULL,
                                    ranking_options = NULL) {
  if (missing(vector_store_ids) || length(vector_store_ids) == 0) {
    rlang::abort("`vector_store_ids` must be a non-empty character vector.")
  }
  tool <- list(
    type = "file_search",
    vector_store_ids = as.list(vector_store_ids)
  )
  if (!is.null(max_num_results)) tool$max_num_results <- max_num_results
  if (!is.null(ranking_options)) {
    if (!is.list(ranking_options)) {
      rlang::abort("`ranking_options` must be a named list.")
    }
    tool$ranking_options <- ranking_options
  }
  tool
}

#' OpenAI built-in code_interpreter tool
#'
#' @description Give the model an OpenAI-hosted Python sandbox.
#'
#' @param container Container spec. Defaults to `list(type = "auto")` which
#'   lets OpenAI provision an ephemeral container. Pass a string container ID
#'   (e.g. `"cntr_abc123"`) to attach to a specific container, or a list for
#'   advanced config (e.g. `list(type = "auto", memory_limit = "4g")`).
#'
#' @return A plain list ready to drop into `tools = list(...)`.
#' @export
#' @examples
#' \dontrun{
#' generate_text(
#'   model,
#'   "Solve 3x + 11 = 14 step by step.",
#'   tools = list(openai_code_interpreter_tool())
#' )
#' }
openai_code_interpreter_tool <- function(container = list(type = "auto")) {
  if (is.character(container) && length(container) == 1) {
    container <- list(type = container)
  }
  if (!is.list(container)) {
    rlang::abort("`container` must be a list or a single string.")
  }
  list(type = "code_interpreter", container = container)
}

#' OpenAI built-in computer_use tool
#'
#' @description Browser / desktop control. The model issues actions
#'   (click, type, scroll, screenshot) which the caller's harness executes
#'   against a virtual computer.
#'
#' @param display_width,display_height Integer screen dimensions in pixels.
#'   Required by the API on most environments.
#' @param environment One of `"browser"`, `"mac"`, `"windows"`, `"ubuntu"`.
#'
#' @return A plain list ready to drop into `tools = list(...)`.
#' @export
openai_computer_use_tool <- function(display_width = NULL,
                                     display_height = NULL,
                                     environment = c("browser", "mac", "windows", "ubuntu")) {
  if (!missing(environment)) {
    environment <- match.arg(environment)
  } else {
    environment <- NULL
  }
  tool <- list(type = "computer_use_preview")
  if (!is.null(display_width))  tool$display_width  <- as.integer(display_width)
  if (!is.null(display_height)) tool$display_height <- as.integer(display_height)
  if (!is.null(environment))    tool$environment    <- environment
  tool
}

#' OpenAI hosted MCP tool
#'
#' @description Give the model access to a remote MCP server, executed
#'   server-side by OpenAI. This is independent of aisdk's local MCP client
#'   (`R/mcp_client.R`) — that one connects from R, this one tells OpenAI to
#'   connect on the model's behalf.
#'
#' @param server_label Required label used to identify the server in tool
#'   calls.
#' @param server_url URL of a custom MCP server. Exactly one of `server_url`
#'   or `connector_id` is required.
#' @param connector_id OpenAI-managed connector ID (e.g. `"connector_gmail"`).
#' @param server_description Optional human-readable description.
#' @param allowed_tools Optional character vector of tool names to expose, or
#'   a list filter (e.g. `list(read_only = TRUE)`).
#' @param require_approval `"always"`, `"never"`, or a list filter
#'   (e.g. `list(always = list(tool_names = c("send_email")))`).
#' @param authorization Optional OAuth access token.
#' @param headers Optional named list of HTTP headers sent to the MCP server.
#'
#' @return A plain list ready to drop into `tools = list(...)`.
#' @export
#' @examples
#' \dontrun{
#' generate_text(
#'   model,
#'   "Roll 2d4+1",
#'   tools = list(openai_hosted_mcp_tool(
#'     server_label = "dmcp",
#'     server_url = "https://dmcp-server.deno.dev/sse",
#'     allowed_tools = "roll",
#'     require_approval = "never"
#'   ))
#' )
#' }
openai_hosted_mcp_tool <- function(server_label,
                                   server_url = NULL,
                                   connector_id = NULL,
                                   server_description = NULL,
                                   allowed_tools = NULL,
                                   require_approval = NULL,
                                   authorization = NULL,
                                   headers = NULL) {
  if (missing(server_label) || !is.character(server_label) || !nzchar(server_label)) {
    rlang::abort("`server_label` is required and must be a non-empty string.")
  }
  if (is.null(server_url) == is.null(connector_id)) {
    rlang::abort("Exactly one of `server_url` or `connector_id` must be provided.")
  }
  tool <- list(type = "mcp", server_label = server_label)
  if (!is.null(server_url))         tool$server_url         <- server_url
  if (!is.null(connector_id))       tool$connector_id       <- connector_id
  if (!is.null(server_description)) tool$server_description <- server_description
  if (!is.null(allowed_tools)) {
    tool$allowed_tools <- if (is.list(allowed_tools)) allowed_tools else as.list(allowed_tools)
  }
  if (!is.null(require_approval))   tool$require_approval   <- require_approval
  if (!is.null(authorization))      tool$authorization      <- authorization
  if (!is.null(headers)) {
    if (!is.list(headers)) {
      rlang::abort("`headers` must be a named list.")
    }
    tool$headers <- headers
  }
  tool
}
