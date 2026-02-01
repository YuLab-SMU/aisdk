#' @title Core Object API: Structured Output Generation
#' @description
#' Functions for generating structured objects from LLMs using schemas.
#' @name core_object

#' @title Generate Object
#' @description
#' Generate a structured R object (list) from a language model based on a schema.
#' The model is instructed to output valid JSON matching the schema, which is
#' then parsed and returned as an R list.
#'
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param prompt A character string prompt describing what to generate.
#' @param schema A schema object created by `z_object()`, `z_array()`, etc.
#' @param schema_name Optional human-readable name for the schema (default: "result").
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (0-2). Default 0.3 (lower for structured output).
#' @param max_tokens Maximum tokens to generate.
#' @param mode Output mode: "json" (prompt-based) or "tool" (function calling).
#'   Currently, only "json" mode is implemented.
#' @param registry Optional ProviderRegistry to use (defaults to global registry).
#' @param ... Additional arguments passed to the model.
#' @return A GenerateObjectResult with:
#'   \itemize{
#'     \item object: The parsed R object (list)
#'     \item usage: Token usage information
#'     \item raw_text: The raw text output from the LLM
#'     \item finish_reason: The reason the generation stopped
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Define a schema for the expected output
#' schema <- z_object(
#'   title = z_string(description = "Title of the article"),
#'   keywords = z_array(z_string()),
#'   sentiment = z_enum(c("positive", "negative", "neutral"))
#' )
#'
#' # Generate structured object
#' result <- generate_object(
#'   model = "openai:gpt-4o",
#'   prompt = "Analyze this article: 'R programming is great for data science!'",
#'   schema = schema
#' )
#'
#' print(result$object$title)
#' print(result$object$sentiment)
#' }
generate_object <- function(model,
                             prompt,
                             schema,
                             schema_name = "result",
                             system = NULL,
                             temperature = 0.3,
                             max_tokens = NULL,
                             mode = c("json", "tool"),
                             registry = NULL,
                             ...) {
  mode <- match.arg(mode)

  # Resolve model from string ID if needed
  model <- resolve_model(model, registry, type = "language")

  # Initialize strategy
  strategy <- ObjectStrategy$new(schema, schema_name)

  # Get schema instruction
  instruction <- strategy$get_instruction()

  # Build system prompt with schema instruction
  combined_system <- if (is.null(system)) {
    instruction
  } else {
    paste0(system, "\n\n", instruction)
  }

  # Call generate_text with the enhanced prompt
  result <- generate_text(
    model = model,
    prompt = prompt,
    system = combined_system,
    temperature = temperature,
    max_tokens = max_tokens,
    ...
  )

  # Parse the result using the strategy
  parsed_object <- strategy$validate(result$text, is_final = TRUE)

  # Return structured result
  structure(
    list(
      object = parsed_object,
      usage = result$usage,
      raw_text = result$text,
      finish_reason = result$finish_reason
    ),
    class = "GenerateObjectResult"
  )
}

#' @title Print GenerateObjectResult
#' @param x A GenerateObjectResult object.
#' @param ... Additional arguments (ignored).
#' @export
print.GenerateObjectResult <- function(x, ...) {
  cat("=== GenerateObjectResult ===\n")
  if (!is.null(x$object)) {
    cat("Object:\n")
    print(x$object)
  } else {
    cat("Object: NULL (parsing failed)\n")
  }
  if (!is.null(x$usage)) {
    cat("\nUsage:\n")
    cat("  Prompt tokens:", x$usage$prompt_tokens, "\n")
    cat("  Completion tokens:", x$usage$completion_tokens, "\n")
    cat("  Total tokens:", x$usage$total_tokens, "\n")
  }
  invisible(x)
}
