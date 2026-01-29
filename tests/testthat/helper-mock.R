
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

    do_generate = function(params) {
      self$last_params <- params # Capture params
      
      if (length(self$responses) == 0) {
        return(list(text = "Mock response", tool_calls = NULL))
      }
      
      # Pop the first response
      resp <- self$responses[[1]]
      self$responses <- self$responses[-1]
      
      # Allow response to be a function of params
      if (is.function(resp)) {
        return(resp(params))
      }
      
      return(resp)
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
