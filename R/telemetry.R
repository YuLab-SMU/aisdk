#' @title Telemetry System
#' @description
#' structured logging and tracing for the AI SDK.
#' @name telemetry
NULL

#' @title Telemetry Class
#' @description
#' R6 class for logging events in a structured format (JSON).
#' @export
Telemetry <- R6::R6Class(
  "Telemetry",
  public = list(
    #' @field trace_id Current trace ID for the session.
    trace_id = NULL,

    #' @field events Collected telemetry events for in-memory inspection.
    events = NULL,

    #' @field emit Logical; when TRUE, events are printed as JSON lines.
    emit = TRUE,
    
    #' @field pricing_table Pricing for common models (USD per 1M tokens).
    pricing_table = list(
      # OpenAI
      "gpt-4o" = list(input = 5.00, output = 15.00),
      "gpt-4o-mini" = list(input = 0.15, output = 0.60),
      # Anthropic
      "claude-3-5-sonnet-20241022" = list(input = 3.00, output = 15.00),
      "claude-3-haiku-20240307" = list(input = 0.25, output = 1.25)
    ),
    
    #' @description Initialize Telemetry
    #' @param trace_id Optional trace ID. If NULL, generates a random one.
    #' @param emit Logical; when TRUE, events are printed as JSON lines.
    initialize = function(trace_id = NULL, emit = TRUE) {
      if (is.null(trace_id)) {
        self$trace_id <- digest(runif(1), algo = "md5")
      } else {
        self$trace_id <- trace_id
      }
      self$events <- list()
      self$emit <- isTRUE(emit)
    },
    
    #' @description Log an event
    #' @param type Event type (e.g., "generation_start", "tool_call").
    #' @param ... Additional fields to log.
    log_event = function(type, ...) {
      event <- list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        trace_id = self$trace_id,
        type = type,
        ...
      )

      self$events <- c(self$events, list(event))

      if (isTRUE(self$emit)) {
        cat(jsonlite::toJSON(event, auto_unbox = TRUE), "\n")
      }
      invisible(event)
    },

    #' @description Return collected telemetry events.
    #' @return A list of event payloads.
    get_events = function() {
      self$events
    },

    #' @description Clear collected telemetry events.
    #' @return Invisibly returns self.
    clear_events = function() {
      self$events <- list()
      invisible(self)
    },
    
    #' @description Create hooks for telemetry
    #' @return A HookHandler object pre-configured with telemetry logs.
    as_hooks = function() {
      create_hooks(
        on_generation_start = function(model, prompt, tools) {
          self$log_event(
            "generation_start",
            model = if (inherits(model, "LanguageModelV1")) model$model else model,
            tool_count = length(tools)
          )
        },
        on_generation_end = function(result) {
          cost <- self$calculate_cost(result)
          
          self$log_event(
            "generation_end",
            status = "success", 
            steps = result$steps %||% 1,
            finish_reason = result$finish_reason,
            usage = result$usage,
            estimated_cost_usd = cost
          )
        },
        on_tool_start = function(tool, args) {
          self$log_event(
            "tool_start",
            tool_name = tool$name,
            success = NULL,
            error = NULL,
            argument_signature = compact_tool_argument_signature(args)
          )
        },
        on_tool_end = function(tool, result, success = TRUE, error = NULL, args = NULL) {
          self$log_event(
            "tool_end",
            tool_name = tool$name,
            success = isTRUE(success),
            error = error %||% NULL,
            error_type = classify_telemetry_error(error),
            argument_signature = compact_tool_argument_signature(args)
          )
        }
      )
    },
    
    #' @description Calculate estimated cost for a generation result
    #' @param result The GenerateResult object.
    #' @param model_id Optional model ID string. if NULL, tries to guess from context (not reliable yet, passing in log_event might be better).
    #' @return Estimated cost in USD, or NULL if unknown.
    calculate_cost = function(result, model_id = NULL) {
      if (is.null(result$usage)) return(NULL)
      
      # TODO: We need the model ID to calculate cost. 
      # Ideally 'result' should contain the model ID used.
      # For now, we will return raw usage if model unknown.
      
      # Simple calculation if we had the rates
      # rate <- self$pricing_table[[model_id]]
      # if (is.null(rate)) return(NULL)
      # (result$usage$prompt_tokens / 1e6 * rate$input) + (result$usage$completion_tokens / 1e6 * rate$output)
      
      NULL
    }
  )
)

#' @keywords internal
compact_tool_argument_signature <- function(args, max_chars = 160) {
  if (is.null(args)) {
    return(NULL)
  }

  payload <- tryCatch(
    safe_to_json(args, auto_unbox = TRUE),
    error = function(e) NULL
  )
  if (is.null(payload)) {
    return(NULL)
  }
  if (nchar(payload) > max_chars) {
    payload <- paste0(substr(payload, 1, max_chars - 3L), "...")
  }
  payload
}

#' @keywords internal
classify_telemetry_error <- function(error) {
  if (is.null(error) || !nzchar(error)) {
    return(NULL)
  }
  lower <- tolower(error)
  if (grepl("unknown .*column|unknown .*assay|unknown .*field|unknown .*dimension|unknown .*seqlevel|missing .*column|does not exist|not found", lower)) {
    return("wrong_accessor")
  }
  if (grepl("hallucinat|nonexistent", lower)) {
    return("hallucinated_field")
  }
  if (grepl("materializ", lower)) {
    return("materialization")
  }
  "tool_error"
}

#' @title Create Telemetry
#' @param trace_id Optional trace ID.
#' @param emit Logical; when TRUE, events are printed as JSON lines.
#' @return A Telemetry object.
#' @export
create_telemetry <- function(trace_id = NULL, emit = TRUE) {
  Telemetry$new(trace_id, emit = emit)
}
