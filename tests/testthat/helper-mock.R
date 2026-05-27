
MockModel <- R6::R6Class("MockModel",
  inherit = LanguageModelV1,
  public = list(
    provider = "mock",
    model_id = "mock-model",
    responses = list(),
    last_params = NULL, # Added to capture parameters

    initialize = function(responses = list()) {
      self$responses <- responses
    },

    next_response = function(params) {
      self$last_params <- params

      if (length(self$responses) == 0) {
        return(list(
          text = "Mock response",
          tool_calls = NULL,
          finish_reason = "stop",
          usage = list(total_tokens = 10)
        ))
      }

      resp <- self$responses[[1]]
      self$responses <- self$responses[-1]

      if (is.function(resp)) {
        return(resp(params))
      }

      resp
    },

    do_generate = function(params) {
      self$next_response(params)
    },

    do_stream = function(params, callback) {
      resp <- self$next_response(params)
      text <- resp$text %||% ""

      if (nzchar(text)) {
        callback(text, TRUE)
      } else {
        callback("", TRUE)
      }

      resp$messages_added <- resp$messages_added %||% list()
      resp
    },
    
    add_response = function(text = NULL, tool_calls = NULL) {
      self$responses <- c(self$responses, list(list(
        text = text,
        tool_calls = tool_calls,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )))
    },
    
    format_tool_result = function(tool_call_id, tool_name, result) {
      list(
        role = "tool",
        tool_call_id = tool_call_id,
        name = tool_name,
        content = result
      )
    }
  )
)
