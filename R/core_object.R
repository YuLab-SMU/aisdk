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
#' @param max_retries Number of times to re-ask the model when its output is
#'   unparseable or missing required schema fields (default 1). The reask
#'   includes the specific validation error. The returned result carries
#'   `valid` and `attempts`.
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
#' \donttest{
#' if (interactive()) {
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
#' }
generate_object <- function(model = NULL,
                             prompt,
                             schema,
                             schema_name = "result",
                             system = NULL,
                             temperature = 0.3,
                             max_tokens = NULL,
                             mode = c("json", "tool"),
                             max_retries = 1L,
                             registry = NULL,
                             ...) {
  mode <- match.arg(mode)
  max_retries <- max(0L, as.integer(max_retries))

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

  debug <- isTRUE(getOption("aisdk.debug", FALSE))
  attempt <- 0L
  reask <- NULL
  result <- NULL
  parsed_object <- NULL
  problem <- NULL

  # Validation + reasking: parse the output, check it against the schema, and
  # on a parse failure or missing required fields re-ask the model once more
  # with the specific error (the "reask" pattern). Native constrained decoding
  # usually gets it right first try; this recovers the rest without failing.
  repeat {
    attempt <- attempt + 1L
    turn_prompt <- if (is.null(reask)) prompt else paste0(prompt, "\n\n", reask)

    result <- generate_text(
      model = model,
      prompt = turn_prompt,
      system = combined_system,
      temperature = temperature,
      max_tokens = max_tokens,
      ...
    )

    if (debug) {
      raw_text <- result$text %||% ""
      message("[DEBUG] generate_object attempt ", attempt, " raw_text (", nchar(raw_text), " chars):\n",
              substr(raw_text, 1, min(1000, nchar(raw_text))),
              if (nchar(raw_text) > 1000) "\n... [truncated]" else "")
    }

    parsed_object <- strategy$validate(result$text, is_final = TRUE)
    problem <- object_schema_problem(parsed_object, schema)

    if (is.null(problem) || attempt > max_retries) {
      break
    }
    if (debug) {
      message("[DEBUG] generate_object: attempt ", attempt, " failed validation (", problem,
              "); reasking.")
    }
    reask <- paste0(
      "Your previous response was not valid: ", problem,
      ". Respond again with ONLY a JSON object that matches the schema exactly."
    )
  }

  # Return structured result
  structure(
    list(
      object = parsed_object,
      usage = result$usage,
      raw_text = result$text,
      finish_reason = result$finish_reason,
      valid = is.null(problem),
      attempts = attempt
    ),
    class = "GenerateObjectResult"
  )
}

#' @keywords internal
# Minimal structural check used to trigger a reask: returns NULL when the parsed
# object looks schema-conformant, or a short human-readable problem string. It
# is intentionally lightweight (unparseable, not an object, or missing a
# required top-level field) — native constrained decoding does the heavy
# lifting; this only catches the common failure modes worth a retry.
object_schema_problem <- function(object, schema) {
  if (is.null(object)) {
    return("the response was not parseable JSON")
  }
  if (!is.list(object) || is.null(names(object))) {
    return("the response was not a JSON object")
  }
  required <- tryCatch(schema_to_list(schema)$required, error = function(e) NULL)
  if (length(required) > 0) {
    missing <- setdiff(unlist(required), names(object))
    if (length(missing) > 0) {
      return(paste0("missing required field(s): ", paste(missing, collapse = ", ")))
    }
  }
  NULL
}

#' @keywords internal
# Strip leading/trailing markdown code fences from (possibly partial) JSON.
strip_json_fences <- function(text) {
  text <- sub("^\\s*```(json)?\\s*", "", text)
  text <- sub("\\s*```\\s*$", "", text)
  trimws(text)
}

#' @title Stream a Structured Object
#' @description
#' Stream a JSON object from the model and parse the growing text into a
#' best-effort PARTIAL object after each chunk (the "streaming structured
#' output" pattern) — so a UI can render fields as they arrive rather than
#' waiting for the whole response. Uses the JSON repair machinery to close the
#' partial JSON on each step.
#'
#' @param model A model (id or object).
#' @param prompt The input prompt.
#' @param schema A `z_schema` describing the desired object.
#' @param schema_name Name for the schema in the instruction.
#' @param on_partial Optional `function(object, done)` called after each chunk
#'   with the current best-effort parsed object (may be incomplete) and a
#'   `done` flag on the final call.
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (default 0.3).
#' @param max_tokens Optional max output tokens.
#' @param registry Optional ProviderRegistry.
#' @param ... Passed to `stream_text()`.
#' @return A `GenerateObjectResult` with the final parsed `object`.
#' @export
stream_object <- function(model = NULL, prompt, schema, schema_name = "result",
                          on_partial = NULL, system = NULL, temperature = 0.3,
                          max_tokens = NULL, registry = NULL, ...) {
  model <- resolve_model(model, registry, type = "language")
  strategy <- ObjectStrategy$new(schema, schema_name)
  combined_system <- if (is.null(system)) {
    strategy$get_instruction()
  } else {
    paste0(system, "\n\n", strategy$get_instruction())
  }

  accumulated <- ""
  last_partial <- NULL
  result <- stream_text(
    model = model,
    prompt = prompt,
    system = combined_system,
    temperature = temperature,
    max_tokens = max_tokens,
    registry = registry,
    callback = function(chunk, done) {
      if (!is.null(chunk) && is.character(chunk)) {
        accumulated <<- paste0(accumulated, chunk)
      }
      # Best-effort parse of the JSON accumulated so far: repair (close open
      # strings/brackets) then parse structure-preserving.
      partial <- tryCatch(
        safe_parse_json(fix_json(strip_json_fences(accumulated)), simplify = FALSE),
        error = function(e) NULL
      )
      if (!is.null(partial)) {
        last_partial <<- partial
      }
      if (is.function(on_partial)) {
        tryCatch(on_partial(last_partial, done = isTRUE(done)), error = function(e) NULL)
      }
    },
    ...
  )

  final <- strategy$validate(result$text, is_final = TRUE)
  structure(
    list(
      object = final %||% last_partial,
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
