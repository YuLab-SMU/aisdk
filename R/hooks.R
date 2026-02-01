#' @title Hooks System
#' @description
#' A system for intercepting and monitoring AI SDK events.
#' Allows implementation of "Human-in-the-loop", logging, and validation.
#' @name hooks
NULL

#' @title Hook Handler
#' @description
#' R6 class to manage and execute hooks.
#' @export
HookHandler <- R6::R6Class(
  "HookHandler",
  public = list(
    #' @field hooks List of hook functions.
    hooks = NULL,
    
    #' @description Initialize HookHandler
    #' @param hooks_list A list of hook functions. Supported hooks:
    #' \itemize{
    #'   \item on_generation_start(model, prompt, tools)
    #'   \item on_generation_end(result)
    #'   \item on_tool_start(tool, args)
    #'   \item on_tool_end(tool, result)
    #'   \item on_tool_approval(tool, args) - Return TRUE to approve, FALSE to deny.
    #' }
    initialize = function(hooks_list = list()) {
      self$hooks <- hooks_list
    },
    
    #' @description Trigger on_generation_start
    #' @param model The language model object.
    #' @param prompt The prompt being sent.
    #' @param tools The list of tools provided.
    trigger_generation_start = function(model, prompt, tools) {
      if (!is.null(self$hooks$on_generation_start)) {
        self$hooks$on_generation_start(model, prompt, tools)
      }
    },
    
    #' @description Trigger on_generation_end
    #' @param result The generation result object.
    trigger_generation_end = function(result) {
      if (!is.null(self$hooks$on_generation_end)) {
        self$hooks$on_generation_end(result)
      }
    },
    
    #' @description Trigger on_tool_start
    #' @param tool The tool object.
    #' @param args The arguments for the tool.
    trigger_tool_start = function(tool, args) {
      # Check approval first
      if (!is.null(self$hooks$on_tool_approval)) {
        approved <- self$hooks$on_tool_approval(tool, args)
        if (!isTRUE(approved)) {
          stop(paste0("Tool execution denied for: ", tool$name))
        }
      }
      
      if (!is.null(self$hooks$on_tool_start)) {
        self$hooks$on_tool_start(tool, args)
      }
    },
    
    #' @description Trigger on_tool_end
    #' @param tool The tool object.
    #' @param result The result from the tool execution.
    trigger_tool_end = function(tool, result) {
      if (!is.null(self$hooks$on_tool_end)) {
        self$hooks$on_tool_end(tool, result)
      }
    }
  )
)

#' @title Create Permission Hook
#' @description
#' Create a hook that enforces a permission mode for tool execution.
#' @param mode Permission mode:
#'   \itemize{
#'     \item "implicit" (default): Auto-approve all tools.
#'     \item "explicit": Ask for confirmation in the console for every tool.
#'     \item "escalate": Ask for confirmation only for tools not in the allowlist.
#'   }
#' @param allowlist List of tool names that are auto-approved in "escalate" mode. 
#'        Default includes read-only tools like "search_web", "read_file".
#' @return A HookHandler object.
#' @export
create_permission_hook <- function(mode = c("implicit", "explicit", "escalate"), 
                                   allowlist = c("search_web", "read_resource", "read_file")) {
  mode <- match.arg(mode)
  
  on_tool_approval <- function(tool, args) {
    if (mode == "implicit") {
      return(TRUE)
    }
    
    if (mode == "escalate") {
      if (tool$name %in% allowlist) {
        return(TRUE)
      }
    }
    
    # Needs explicit confirmation
    cat(sprintf("\n[Permission Required] Tool: %s\nArguments:\n%s\n", 
                tool$name, 
                jsonlite::toJSON(args, auto_unbox = TRUE, pretty = TRUE)))
    
    if (interactive()) {
      response <- readline(prompt = "Approve execution? (y/n): ")
      return(tolower(response) == "y")
    } else {
      warning("Non-interactive session: Denying tool execution requiring permission.")
      return(FALSE)
    }
  }
  
  HookHandler$new(list(on_tool_approval = on_tool_approval))
}

#' @title Create Hooks
#' @description
#' Helper to create a HookHandler from a list of functions.
#' @param ... Named arguments matching supported hook names.
#' @return A HookHandler object.
#' @export
create_hooks <- function(...) {
  HookHandler$new(list(...))
}
