#' @title Output Strategy System
#' @description
#' Implements the Strategy pattern for handling different structured output
#' formats from LLMs. This allows the SDK to be extended with new output types
#' (e.g., objects, enums, dataframes) without modifying core logic.
#' @name strategy
NULL

#' Output Strategy Interface
#'
#' Abstract R6 class defining the interface for output strategies.
#' Subclasses must implement `get_instruction()` and `validate()`.
#' @export
OutputStrategy <- R6::R6Class(
  "OutputStrategy",
  public = list(
    #' @description Initialize the strategy.
    initialize = function() {},

    #' @description
    #' Get the system prompt instruction for this strategy.
    #' @return A character string with instructions for the LLM.
    get_instruction = function() {
      stop("Abstract method: get_instruction must be implemented by subclass")
    },

    #' @description
    #' Parse and validate the output text.
    #' @param text The raw text output from the LLM.
    #' @param is_final Logical, TRUE if this is the final output (not streaming).
    #' @return The parsed and validated object.
    validate = function(text, is_final = FALSE) {
      stop("Abstract method: validate must be implemented by subclass")
    }
  )
)

#' Object Strategy
#'
#' Strategy for generating structured objects based on a JSON Schema.
#' This strategy instructs the LLM to produce valid JSON matching the schema,
#' and handles parsing and validation of the output.
#' @export
ObjectStrategy <- R6::R6Class(
  "ObjectStrategy",
  inherit = OutputStrategy,
  public = list(
    #' @field schema The schema definition (from z_object, etc.).
    schema = NULL,
    #' @field schema_name Human-readable name for the schema.
    schema_name = NULL,

    #' @description
    #' Initialize the ObjectStrategy.
    #' @param schema A schema object created by z_object, z_array, etc.
    #' @param schema_name An optional name for the schema (default: "json_schema").
    initialize = function(schema, schema_name = "json_schema") {
      self$schema <- schema
      self$schema_name <- schema_name
    },

    #' @description
    #' Generate the instruction for the LLM to output valid JSON.
    #' @return A character string with the prompt instruction.
    get_instruction = function() {
      json_schema_str <- schema_to_json(self$schema)

      paste0(
        "You must generate a valid JSON object that matches the following schema:\n",
        "Schema Name: ", self$schema_name, "\n",
        "JSON Schema:\n", json_schema_str, "\n\n",
        "IMPORTANT: Output ONLY the raw JSON object. Do not include any explanations, ",
        "markdown code blocks (like ```json), or any other text outside of the JSON."
      )
    },

    #' @description
    #' Validate and parse the LLM output as JSON.
    #' @param text The raw text output from the LLM.
    #' @param is_final Logical, TRUE if this is the final output.
    #' @return The parsed R object (list), or NULL if parsing fails.
    validate = function(text, is_final = FALSE) {
      if (is.null(text) || nchar(trimws(text)) == 0) {
        return(NULL)
      }

      # Remove potential Markdown code block markers
      clean_text <- text
      # Remove ```json at the start and ``` at the end
      clean_text <- gsub("^\\s*```json\\s*", "", clean_text)
      clean_text <- gsub("^\\s*```\\s*", "", clean_text) # generic code block
      clean_text <- gsub("\\s*```\\s*$", "", clean_text)
      clean_text <- trimws(clean_text)

      # Use safe_parse_json which handles repair
      obj <- safe_parse_json(clean_text)

      # TODO: In the future, add schema validation here using a JSON Schema validator
      # For now, we just return the parsed object

      return(obj)
    }
  )
)
