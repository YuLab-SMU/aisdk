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
    
    #' @description Trigger on_generation_start. Acts as an input guardrail: if
    #'   the hook returns a revised prompt (a character string, or a non-empty
    #'   list of message objects), that revision is used; any other return value
    #'   (including `NULL` from an observe-only hook) leaves the prompt
    #'   unchanged. To block a turn, the hook can `stop()`.
    #' @param model The language model object.
    #' @param prompt The prompt being sent.
    #' @param tools The list of tools provided.
    #' @return The prompt to use (revised or original).
    trigger_generation_start = function(model, prompt, tools) {
      if (!is.null(self$hooks$on_generation_start)) {
        revised <- self$hooks$on_generation_start(model, prompt, tools)
        if (is.character(revised) && length(revised) > 0) {
          return(revised)
        }
        if (is.list(revised) && length(revised) > 0 &&
            all(vapply(revised, function(m) is.list(m) && !is.null(m$role), logical(1)))) {
          return(revised)
        }
      }
      prompt
    },

    #' @description Trigger on_generation_end. Acts as an output guardrail: if
    #'   the hook returns a `GenerateResult` (e.g. a revised or refusal result),
    #'   that replaces the result; any other return value (including `NULL` from
    #'   an observe-only hook) leaves the result unchanged.
    #' @param result The generation result object.
    #' @return The result to use (revised or original).
    trigger_generation_end = function(result) {
      if (!is.null(self$hooks$on_generation_end)) {
        revised <- self$hooks$on_generation_end(result)
        # Accept a GenerateResult replacement (real providers) or a result-
        # shaped list carrying `text` (mocks / list results). Anything else
        # (NULL from an observe-only hook) leaves the result unchanged.
        looks_like_result <- inherits(revised, "GenerateResult") ||
          (is.list(revised) && "text" %in% names(revised))
        if (looks_like_result) {
          return(revised)
        }
      }
      result
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
    #' @param success Logical indicating whether execution succeeded.
    #' @param error Optional error message when execution failed.
    #' @param args Optional tool arguments for downstream telemetry.
    trigger_tool_end = function(tool, result, success = TRUE, error = NULL, args = NULL) {
      if (!is.null(self$hooks$on_tool_end)) {
        fn <- self$hooks$on_tool_end
        fmls <- names(formals(fn))
        if (is.null(fmls)) {
          fmls <- character(0)
        }
        if ("..." %in% fmls || length(fmls) >= 5) {
          fn(tool, result, success, error, args)
        } else if (length(fmls) >= 4) {
          fn(tool, result, success, error)
        } else if (length(fmls) >= 3) {
          fn(tool, result, success)
        } else {
          fn(tool, result)
        }
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
