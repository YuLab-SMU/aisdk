#' @title Tool System: Function Calling Support
#' @description
#' Tool class and utilities for LLM function calling. Provides a bridge between
#' LLM tool call requests and R functions.
#'
#' Implements multi-layer defense strategy for handling incomplete or malformed
#' tool call parameters from LLMs (inspired by Opencode):
#' - Tool call repair mechanism (name case fixing, invalid tool routing)
#' - JSON parsing fault tolerance at multiple layers
#' - Parameter normalization with safe defaults
#' - Graceful degradation on parse failures
#'
#' @importFrom stats setNames
#' @name tool

NULL

#' @title Parse Tool Arguments
#' @description
#' Robustly parse tool call arguments from various formats that different LLMs may return.
#' Handles edge cases like incomplete JSON, malformed strings, and various empty representations.
#'
#' Implements multi-layer parsing strategy (inspired by Opencode):
#' 1. Direct pass-through for already-parsed lists
#' 2. Empty value detection and normalization
#' 3. JSON repair for common LLM mistakes
#' 4. Fallback parsing with JavaScript object literal support
#' 5. Graceful degradation to empty args on failure
#'
#' @param args The arguments to parse (can be string, list, or NULL).
#' @param tool_name Optional tool name for better error messages.
#' @return A named list of parsed arguments (empty named list if no arguments).
#' @keywords internal
parse_tool_arguments <- function(args, tool_name = "unknown") {
  # Helper to return empty named list (serializes to {} not [])
  empty_args <- function() stats::setNames(list(), character(0))

  # Case 1: Already a list - ensure it's named for proper JSON serialization

  if (is.list(args)) {
    if (length(args) == 0) return(empty_args())
    return(args)
  }

  # Case 2: NULL or NA - return empty named list
  if (is.null(args) || (length(args) == 1 && is.na(args))) {
    return(empty_args())
  }

  # Case 3: Not a character string - wrap in list
  if (!is.character(args)) {
    return(list(value = args))
  }

  # Case 4: Character string - need to parse
  args_str <- trimws(args)

  # Case 4a: Empty or known empty representations
  # Handle various ways models might represent "no arguments"
  empty_patterns <- c(
    "",           # Empty string
    "{}",         # Empty JSON object
    "{ }",        # Empty JSON object with space
    "null",       # JSON null
    "NULL",       # R-style NULL
    "undefined",  # JavaScript undefined
    "{",          # Incomplete JSON (some models like glm-4)
    "}",          # Just closing brace
    "[]",         # Empty array (some models)
    "[ ]"         # Empty array with space
  )

  if (args_str %in% empty_patterns) {
    return(empty_args())
  }

  # Case 4b: Try to repair common JSON issues before parsing
  repaired_args <- repair_json_string(args_str)

  # Case 4c: Try to parse as JSON with multi-layer fallback
  result <- tryCatch(
    jsonlite::fromJSON(repaired_args, simplifyVector = FALSE),
    error = function(e) {
      # Fallback 1: Try advanced JSON repair
      fixed_args <- fix_json(args_str)
      tryCatch(
        jsonlite::fromJSON(fixed_args, simplifyVector = FALSE),
        error = function(e2) {
          # Fallback 2: Try JavaScript object literal evaluation (like Opencode)
          tryCatch({
            # This handles cases like {key: "value"} without quotes on keys
            # Convert to valid JSON first
            js_to_json <- gsub("([{,])\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:", '\\1"\\2":', args_str)
            js_to_json <- fix_json(js_to_json)
            jsonlite::fromJSON(js_to_json, simplifyVector = FALSE)
          }, error = function(e3) {
            # Log warning but don't fail
            debug_log("parse_tool_arguments", list(
              tool = tool_name,
              raw = substr(args_str, 1, 100),
              repaired = substr(repaired_args, 1, 100),
              error = conditionMessage(e)
            ))
            # Return empty named list as fallback
            empty_args()
          })
        }
      )
    }
  )

  # Ensure result is a list
  if (!is.list(result)) {
    result <- list(value = result)
  }

  # Ensure empty list is named
  if (length(result) == 0) {
    result <- empty_args()
  }

  result
}

#' @title Repair JSON String
#' @description
#' Attempt to repair common JSON malformations from LLM outputs.
#' This is a lightweight repair function for common issues.
#' For more complex repairs, use fix_json() from utils_json.R.
#'
#' Handles:
#' - Missing closing braces/brackets
#' - Trailing commas
#' - Unquoted keys
#' - Truncated strings
#' - Single quotes instead of double quotes
#'
#' @param json_str The potentially malformed JSON string.
#' @return A repaired JSON string (best effort).
#' @keywords internal
repair_json_string <- function(json_str) {
  if (!is.character(json_str) || length(json_str) != 1) {
    return("{}")
  }

  s <- trimws(json_str)

  # Empty or very short strings

  if (nchar(s) == 0) return("{}")
  if (s == "{") return("{}")
  if (s == "}") return("{}")
  if (s == "[") return("[]")
  if (s == "]") return("[]")

  # Replace single quotes with double quotes (common LLM mistake)
  # But be careful not to replace apostrophes in strings
  # Simple heuristic: replace 'key': with "key":
  s <- gsub("'([^']+)'\\s*:", '"\\1":', s)

  # Count braces to detect truncation
  open_braces <- nchar(gsub("[^{]", "", s))
  close_braces <- nchar(gsub("[^}]", "", s))
  open_brackets <- nchar(gsub("[^\\[]", "", s))
  close_brackets <- nchar(gsub("[^\\]]", "", s))

  # Add missing closing braces/brackets
  if (open_braces > close_braces) {
    s <- paste0(s, paste(rep("}", open_braces - close_braces), collapse = ""))
  }
  if (open_brackets > close_brackets) {
    s <- paste0(s, paste(rep("]", open_brackets - close_brackets), collapse = ""))
  }

  # Handle trailing comma before closing brace (common LLM mistake)
  # e.g., {"a": 1,} -> {"a": 1}
  s <- gsub(",\\s*}", "}", s)
  s <- gsub(",\\s*\\]", "]", s)

  # Handle unquoted keys (some models do this)
  # This is a simple heuristic, not perfect
  # e.g., {key: "value"} -> {"key": "value"}
  s <- gsub('([{,])\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:', '\\1"\\2":', s)

  # Handle truncated string values (missing closing quote)
  # Count quotes - if odd number, add one at the end before closing brace
  quote_count <- nchar(gsub('[^"]', "", s))
  if (quote_count %% 2 == 1) {
    # Find the last closing brace/bracket and insert quote before it
    s <- gsub('([^"\\s])([}\\]])$', '\\1"\\2', s)
  }

  s
}

# ============================================================================
# Tool Call Repair Mechanism (Inspired by Opencode)
# ============================================================================

#' @title Repair Tool Call
#' @description
#' Attempts to repair a failed tool call. This implements a multi-layer repair
#' strategy inspired by Opencode's experimental_repairToolCall:
#' 1. Try to fix tool name case issues (e.g., "GetWeather" -> "get_weather")
#' 2. If repair fails, route to an "invalid" tool for graceful handling
#'
#' @param tool_call A list with name, arguments, and optionally id.
#' @param tools A list of available Tool objects.
#' @param error_message Optional error message from the failed call.
#' @return A repaired tool call list, or an "invalid" tool call if unrepairable.
#' @keywords internal
#' @examples
#' \dontrun{
#' # If LLM calls "GetWeather" but tool is "get_weather"
#' repaired <- repair_tool_call(
#'   list(name = "GetWeather", arguments = list(city = "Tokyo")),
#'   tools = list(get_weather_tool)
#' )
#' }
repair_tool_call <- function(tool_call, tools, error_message = NULL) {
  original_name <- tool_call$name


  # Strategy 1: Try lowercase version of tool name
  lower_name <- tolower(original_name)
  if (lower_name != original_name) {
    tool_obj <- find_tool(tools, lower_name)
    if (!is.null(tool_obj)) {
      debug_log("repair_tool_call", list(
        original = original_name,
        repaired = lower_name,
        strategy = "lowercase"
      ))
      tool_call$name <- lower_name
      return(tool_call)
    }
  }

  # Strategy 2: Try snake_case conversion (e.g., "getWeather" -> "get_weather")
  snake_name <- to_snake_case(original_name)
  if (snake_name != original_name && snake_name != lower_name) {
    tool_obj <- find_tool(tools, snake_name)
    if (!is.null(tool_obj)) {
      debug_log("repair_tool_call", list(
        original = original_name,
        repaired = snake_name,
        strategy = "snake_case"
      ))
      tool_call$name <- snake_name
      return(tool_call)
    }
  }

  # Strategy 3: Try fuzzy matching (find closest tool name)
  tool_names <- sapply(tools, function(t) t$name)
  closest <- find_closest_match(original_name, tool_names)
  if (!is.null(closest) && closest != original_name) {
    debug_log("repair_tool_call", list(
      original = original_name,
      repaired = closest,
      strategy = "fuzzy_match"
    ))
    tool_call$name <- closest
    return(tool_call)
  }

  # Strategy 4: Route to "invalid" tool for graceful handling
  debug_log("repair_tool_call", list(
    original = original_name,
    strategy = "route_to_invalid",
    error = error_message
  ))

  list(
    id = tool_call$id %||% paste0("invalid_", substr(digest::digest(Sys.time()), 1, 8)),
    name = "__invalid__",
    arguments = list(
      original_tool = original_name,
      original_arguments = tool_call$arguments,
      error = error_message %||% paste0("Tool '", original_name, "' not found")
    )
  )
}

#' @title Convert to Snake Case
#' @description Convert camelCase or PascalCase to snake_case.
#' @param x A character string.
#' @return Snake case version of the string.
#' @keywords internal
to_snake_case <- function(x) {
  if (!is.character(x) || length(x) != 1) return(x)
  # Insert underscore before uppercase letters, then lowercase
  s <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(s)
}

#' @title Find Closest Match
#' @description Find the closest matching string using Levenshtein distance.
#' @param target The target string to match.
#' @param candidates A vector of candidate strings.
#' @param max_distance Maximum allowed edit distance (default 3).
#' @return The closest match, or NULL if none within max_distance.
#' @keywords internal
find_closest_match <- function(target, candidates, max_distance = 3) {
  if (length(candidates) == 0) return(NULL)

  target_lower <- tolower(target)
  candidates_lower <- tolower(candidates)

  # Calculate edit distances
  distances <- sapply(candidates_lower, function(c) {
    utils::adist(target_lower, c)[1, 1]
  })

  min_dist <- min(distances)
  if (min_dist <= max_distance) {
    return(candidates[which.min(distances)])
  }

  NULL
}

#' @title Create Invalid Tool Handler
#' @description
#' Creates a special "__invalid__" tool that handles unrecognized or failed
#' tool calls gracefully. This allows the system to continue operating and
#' provide meaningful feedback to the LLM.
#' @return A Tool object for handling invalid tool calls.
#' @keywords internal
create_invalid_tool_handler <- function() {
  Tool$new(
    name = "__invalid__",
    description = "Internal handler for invalid or unrecognized tool calls",
    parameters = z_object(
      original_tool = z_string(description = "The original tool name that was called"),
      original_arguments = z_any_object(description = "The original arguments passed"),
      error = z_string(description = "Error message describing why the call failed")
    ),
    execute = function(args) {
      # Return a structured error message that helps the LLM understand what went wrong
      error_response <- list(
        success = FALSE,
        error_type = "invalid_tool_call",
        message = sprintf(
          "Tool '%s' is not available. %s",
          args$original_tool %||% "unknown",
          args$error %||% "Please check the tool name and try again."
        ),
        suggestion = "Please use one of the available tools listed in your instructions."
      )

      safe_to_json(error_response, auto_unbox = TRUE)
    }
  )
}

#' @title Debug Log Helper
#' @description Log debug information if debug mode is enabled.
#' @param context A string describing the context.
#' @param data A list of data to log.
#' @keywords internal
debug_log <- function(context, data) {
  debug_opt <- getOption("aisdk.debug", FALSE)
  debug_enabled <- isTRUE(debug_opt) ||
    (is.character(debug_opt) && tolower(debug_opt) %in% c("1", "true", "yes", "on"))
  debug_env <- Sys.getenv("AISDK_DEBUG", "")
  if (nzchar(debug_env) && tolower(debug_env) %in% c("1", "true", "yes", "on")) {
    debug_enabled <- TRUE
  }

  if (debug_enabled) {
    message(sprintf("aisdk debug [%s]: %s", context, safe_to_json(data, auto_unbox = TRUE)))
  }
}

#' @title Tool Class
#' @description
#' R6 class representing a callable tool for LLM function calling.
#' A Tool connects an LLM's tool call request to an R function.
#' @export
Tool <- R6::R6Class(
  "Tool",
  public = list(
    #' @field name The unique name of the tool.
    name = NULL,

    #' @field description A description of what the tool does.
    description = NULL,

    #' @field parameters A z_object schema defining the tool's parameters.
    parameters = NULL,

    #' @field layer Tool layer: "llm" (loaded into context) or "computer" (executed via bash/filesystem).
    layer = "llm",

    #' @description Initialize a Tool.
    #' @param name Unique tool name (used by LLM to call the tool).
    #' @param description Description of the tool's purpose.
    #' @param parameters A z_object schema defining expected parameters.
    #' @param execute An R function that implements the tool logic.
    #' @param layer Tool layer: "llm" or "computer" (default: "llm").
    initialize = function(name, description, parameters, execute, layer = "llm") {
      if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
        rlang::abort("Tool name must be a non-empty string")
      }
      if (!is.character(description) || length(description) != 1) {
        rlang::abort("Tool description must be a string")
      }
      if (!inherits(parameters, "z_schema")) {
        rlang::abort("Tool parameters must be a z_schema object (use z_object())")
      }
      if (!is.function(execute)) {
        rlang::abort("Tool execute must be a function")
      }
      if (!layer %in% c("llm", "computer")) {
        rlang::abort("Tool layer must be 'llm' or 'computer'")
      }

      self$name <- name
      self$description <- description
      self$parameters <- parameters
      self$layer <- layer
      private$.execute <- execute
    },
    
    #' @description Convert tool to API format.
    #' @param provider Provider name ("openai" or "anthropic"). Default "openai".
    #' @return A list in the format expected by the API.
    to_api_format = function(provider = "openai") {
      # Convert schema to plain list for JSON serialization
      params_list <- schema_to_list(self$parameters)
      
      if (provider == "anthropic") {
        # Anthropic format
        list(
          name = self$name,
          description = self$description,
          input_schema = params_list
        )
      } else {
        # OpenAI format (default)
        list(
          type = "function",
          `function` = list(
            name = self$name,
            description = self$description,
            parameters = params_list
          )
        )
      }
    },
    
    #' @description Execute the tool with given arguments.
    #' @param args A list or named list of arguments.
    #' @param envir Optional environment in which to evaluate the tool function.
    #'   When provided, the environment is passed as `.envir` in the args list,
    #'   allowing the execute function to access and modify session variables.
    #' @return The result of executing the tool function.
    run = function(args, envir = NULL) {
      # Use the robust argument parser
      args <- parse_tool_arguments(args, tool_name = self$name)

      # Pass the environment as a special .envir argument if provided
      if (!is.null(envir) && is.environment(envir)) {
        args$.envir <- envir
      }

      private$.execute(args)
    },
    
    #' @description Print method for Tool.
    print = function() {
      cat("<Tool>\n")
      cat("  Name:", self$name, "\n")
      cat("  Description:", self$description, "\n")
      cat("  Parameters:\n")
      cat("    ", gsub("\n", "\n    ", schema_to_json(self$parameters, pretty = TRUE)), "\n")
      invisible(self)
    }
  ),
  
  private = list(
    .execute = NULL
  )
)

# ============================================================================
# Tool Factory Helpers (Smart Parameters + Execute Normalization)
# ============================================================================

looks_like_raw_schema <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(FALSE)
  schema_keys <- c("type", "properties", "required", "additionalProperties", "items", "enum",
                   "oneOf", "anyOf", "allOf")
  has_schema_key <- any(names(x) %in% schema_keys)
  has_non_schema <- any(!names(x) %in% schema_keys)
  has_z_schema_values <- any(vapply(x, inherits, logical(1), "z_schema"))
  has_schema_key && !has_non_schema && !has_z_schema_values
}

infer_tool_schema_from_execute <- function(execute) {
  if (!is.function(execute)) return(NULL)
  fmls <- formals(execute)
  fml_names <- names(fmls) %||% character(0)

  # If this is list-style execute(args), schema must be supplied manually.
  if (length(fml_names) >= 1 && fml_names[1] == "args") {
    return(NULL)
  }

  if (length(fml_names) == 0) {
    return(z_empty_object())
  }

  schema <- create_schema_from_func(
    execute,
    exclude_args = ".envir",
    type_mode = "any"
  )

  if ("..." %in% fml_names) {
    schema$additionalProperties <- TRUE
  }

  schema
}

coerce_tool_parameters <- function(parameters, execute = NULL) {
  if (inherits(parameters, "z_schema")) return(parameters)

  if (is.null(parameters)) {
    inferred <- infer_tool_schema_from_execute(execute)
    if (!is.null(inferred)) return(inferred)
    return(z_any_object(description = "Free-form arguments"))
  }

  if (is.character(parameters)) {
    if (length(parameters) == 0) return(z_empty_object())
    if (!is.null(names(parameters)) && any(nzchar(names(parameters)))) {
      prop_names <- names(parameters)
      prop_desc <- as.list(parameters)
    } else {
      prop_names <- parameters
      prop_desc <- as.list(paste("Parameter", parameters))
    }
    props <- lapply(prop_desc, function(desc) z_any(description = desc))
    names(props) <- prop_names
    return(do.call(z_object, c(props, list(.required = prop_names))))
  }

  if (is.list(parameters)) {
    if (looks_like_raw_schema(parameters)) {
      rlang::abort("Tool parameters must be a z_schema object (use z_object())")
    }
    if (length(parameters) == 0) return(z_empty_object())
    if (is.null(names(parameters)) || any(names(parameters) == "")) {
      rlang::abort("Tool parameters as list must be a named list")
    }

    props <- list()
    for (name in names(parameters)) {
      val <- parameters[[name]]
      if (inherits(val, "z_schema")) {
        props[[name]] <- val
      } else if (is.character(val) && length(val) == 1) {
        props[[name]] <- z_any(description = val)
      } else if (is.null(val)) {
        props[[name]] <- z_any(description = paste("Parameter", name), nullable = TRUE, default = NULL)
      } else if (is.numeric(val) || is.logical(val)) {
        props[[name]] <- z_any(description = paste("Parameter", name), default = val)
      } else if (is.character(val) && length(val) > 1) {
        props[[name]] <- z_any(description = paste("Parameter", name), default = val[[1]])
      } else {
        props[[name]] <- z_any(description = paste("Parameter", name))
      }
    }

    return(do.call(z_object, c(props, list(.required = names(props)))))
  }

  rlang::abort("Tool parameters must be a z_schema object, named list, character vector, or NULL")
}

normalize_tool_execute <- function(execute) {
  if (!is.function(execute)) return(execute)
  fmls <- formals(execute)
  fml_names <- names(fmls) %||% character(0)

  # Preserve existing list-style tools: execute(args, ...)
  if (length(fml_names) >= 1 && fml_names[1] == "args") {
    return(execute)
  }

  function(args) {
    if (!is.list(args)) args <- list()

    # Drop .envir unless explicitly requested
    if (!(".envir" %in% fml_names) && ".envir" %in% names(args)) {
      args$.envir <- NULL
      args <- args[names(args) != ".envir"]
    }

    if (length(fml_names) == 0) {
      return(execute())
    }

    if ("..." %in% fml_names) {
      return(do.call(execute, args))
    }

    if (is.null(names(args)) || length(args) == 0) {
      return(do.call(execute, list()))
    }

    filtered <- args[names(args) %in% fml_names]
    do.call(execute, filtered)
  }
}

#' @title Create a Tool
#' @description
#' Factory function to create a Tool object. This is the recommended way
#' to define tools for LLM function calling.
#' @param name Unique tool name (used by LLM to call the tool).
#' @param description Description of the tool's purpose. Be descriptive
#'   to help the LLM understand when to use this tool.
#' @param parameters A z_schema object (z_object/z_any/etc), a named list,
#'   a character vector, or NULL. When NULL, the schema is inferred from the
#'   execute function signature (if possible) and defaults to flexible types.
#' @param execute An R function that implements the tool logic. It can accept
#'   a single list argument (args), or standard named parameters.
#'   List-style functions receive a single list argument containing parameters.
#' @param layer Tool layer: "llm" (loaded into context) or "computer" (executed via bash/filesystem).
#'   Default is "llm". Computer layer tools are not loaded into context but executed via bash.
#' @return A Tool object.
#' @rdname tool_factory
#' @export
#' @examples
#' \dontrun{
#' # Define a weather tool
#' get_weather <- tool(
#'   name = "get_weather",
#'   description = "Get the current weather for a location",
#'   parameters = z_object(
#'     location = z_string(description = "The city name, e.g., 'Beijing'"),
#'     unit = z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
#'   ),
#'   execute = function(args) {
#'     # In real usage, call a weather API here
#'     paste("Weather in", args$location, "is 22 degrees", args$unit)
#'   }
#' )
#'
#' # Use with generate_text
#' result <- generate_text(
#'   model = "openai:gpt-4o",
#'   prompt = "What's the weather in Tokyo?",
#'   tools = list(get_weather)
#' )
#' }
tool <- function(name, description, parameters = NULL, execute = NULL, layer = "llm") {
  if (is.function(parameters) && is.null(execute)) {
    execute <- parameters
    parameters <- NULL
  }
  parameters <- coerce_tool_parameters(parameters, execute = execute)
  execute <- normalize_tool_execute(execute)
  Tool$new(
    name = name,
    description = description,
    parameters = parameters,
    execute = execute,
    layer = layer
  )
}

# ============================================================================
# Tool Result Helpers
# ============================================================================

#' @title Create Tool Result Message
#' @description
#' Create a message representing the result of a tool call.
#' Used to send tool execution results back to the LLM.
#' @param tool_call_id The ID of the tool call this result responds to.
#' @param result The result content (will be converted to string if needed).
#' @param is_error If TRUE, indicates this result is an error message.
#' @return A list representing a tool result message.
#' @export
#' @keywords internal
tool_result_message <- function(tool_call_id, result, is_error = FALSE) {
  # Convert result to string if needed
  if (!is.character(result)) {
    result <- safe_to_json(result, auto_unbox = TRUE)
  }
  
  list(
    role = "tool",
    tool_call_id = tool_call_id,
    content = result
  )
}

#' @title Find Tool by Name
#' @description
#' Find a tool in a list of tools by its name.
#' @param tools A list of Tool objects.
#' @param name The tool name to find.
#' @return The Tool object if found, NULL otherwise.
#' @keywords internal
find_tool <- function(tools, name) {
  for (t in tools) {
    if (t$name == name) {
      return(t)
    }
  }
  NULL
}

#' @title Execute Tool Calls
#' @description
#' Execute a list of tool calls returned by an LLM. This function safely
#' executes each tool, handling errors gracefully and returning a standardized
#' result format.
#'
#' Implements multi-layer defense strategy:
#' 1. Tool name repair (case fixing, snake_case conversion, fuzzy matching)
#' 2. Invalid tool routing for graceful degradation
#' 3. Argument parsing with JSON repair
#' 4. Error capture and structured error responses
#'
#' @param tool_calls A list of tool call objects, each with id, name, and arguments.
#' @param tools A list of Tool objects to search for matching tools.
#' @param hooks Optional HookHandler object.
#' @param envir Optional environment in which to execute tools. When provided,
#'   tool functions can access and modify variables in this environment,
#'   enabling cross-agent data sharing through a shared session environment.
#' @param repair_enabled Whether to attempt tool call repair (default TRUE).
#' @return A list of execution results, each containing:
#'   \itemize{
#'     \item id: The tool call ID
#'     \item name: The tool name
#'     \item result: The execution result (or error message)
#'     \item is_error: TRUE if an error occurred during execution
#'   }
#' @export
execute_tool_calls <- function(tool_calls, tools, hooks = NULL, envir = NULL,
                               repair_enabled = TRUE) {
  if (is.null(tool_calls) || length(tool_calls) == 0) {
    return(list())
  }

  debug_opt <- getOption("aisdk.debug", FALSE)
  debug_enabled <- isTRUE(debug_opt) || (is.character(debug_opt) && tolower(debug_opt) %in% c("1", "true", "yes", "on"))
  debug_env <- Sys.getenv("AISDK_DEBUG", "")
  if (nzchar(debug_env) && tolower(debug_env) %in% c("1", "true", "yes", "on")) {
    debug_enabled <- TRUE
  }

  # Add invalid tool handler to tools list for graceful degradation
  tools_with_invalid <- c(tools, list(create_invalid_tool_handler()))

  results <- lapply(tool_calls, function(tc) {
    if (isTRUE(debug_enabled)) {
      debug_payload <- list(
        id = tc$id,
        name = tc$name,
        arguments = tc$arguments,
        arguments_type = class(tc$arguments),
        arguments_names = names(tc$arguments)
      )
      message("aisdk debug: tool_call=", safe_to_json(debug_payload, auto_unbox = TRUE))
    }

    # Find the tool by name
    tool_obj <- find_tool(tools, tc$name)

    # If tool not found and repair is enabled, try to repair
    if (is.null(tool_obj) && isTRUE(repair_enabled)) {
      repaired_tc <- repair_tool_call(tc, tools, error_message = NULL)
      tc <- repaired_tc
      tool_obj <- find_tool(tools_with_invalid, tc$name)
    }

    if (is.null(tool_obj)) {
      return(list(
        id = tc$id,
        name = tc$name,
        result = paste0("Error: Tool '", tc$name, "' not found"),
        is_error = TRUE
      ))
    }

    # Execute the tool with error handling (including hook errors)
    result_list <- tryCatch({
      # Trigger tool start hook (may throw if approval denied)
      if (!is.null(hooks)) {
        hooks$trigger_tool_start(tool_obj, tc$arguments)
      }

      result <- tool_obj$run(tc$arguments, envir = envir)

      # Convert result to string if needed
      if (!is.character(result)) {
        result <- safe_to_json(result, auto_unbox = TRUE)
      }

      # Auto-recovery for skill script errors (soft failures)
      if (tc$name == "execute_skill_script" && is_skill_script_error(result)) {
        recovered <- try_recover_skill_tool_call(tc, tools, envir)
        if (!is.null(recovered)) {
          result <- recovered$result
        } else {
          return(list(
            id = tc$id,
            name = tc$name,
            result = result,
            is_error = TRUE
          ))
        }
      }

      # Trigger tool end hook only on success
      if (!is.null(hooks)) {
        hooks$trigger_tool_end(tool_obj, result)
      }

      list(
        id = tc$id,
        name = tc$name,
        result = result,
        is_error = FALSE
      )
    }, error = function(e) {
      if (isTRUE(debug_enabled)) {
        err_payload <- list(
          id = tc$id,
          name = tc$name,
          error = conditionMessage(e),
          arguments = tc$arguments
        )
        message("aisdk debug: tool_error=", safe_to_json(err_payload, auto_unbox = TRUE))
      }
      list(
        id = tc$id,
        name = tc$name,
        result = paste0("Error executing tool '", tc$name, "': ", conditionMessage(e)),
        is_error = TRUE
      )
    })

    result_list
  })

  results
}

# ============================================================================
# Tool Auto-Recovery Helpers
# ============================================================================

is_skill_script_error <- function(result) {
  if (!is.character(result)) return(FALSE)
  grepl("^Script execution error:", result) ||
    grepl("^Error executing tool", result) ||
    grepl("^Error:", result) ||
    grepl("Object not found:", result)
}

try_recover_skill_tool_call <- function(tool_call, tools, envir = NULL) {
  tool_obj <- find_tool(tools, "execute_skill_script")
  if (is.null(tool_obj)) return(NULL)

  parsed <- parse_tool_arguments(tool_call$arguments, tool_name = "execute_skill_script")
  variants <- build_skill_arg_variants(parsed)

  for (variant in variants) {
    res <- run_tool_safely(tool_obj, variant, envir = envir)
    if (!is_skill_script_error(res$result)) {
      return(list(
        id = tool_call$id,
        name = tool_call$name,
        result = res$result,
        is_error = FALSE
      ))
    }
  }

  # Fallback: attempt direct R execution if available
  exec_tool <- find_tool(tools, "execute_r_code")
  if (!is.null(exec_tool)) {
    fallback <- build_skill_fallback_code(parsed)
    if (!is.null(fallback)) {
      res <- run_tool_safely(exec_tool, fallback, envir = envir)
      if (!is_skill_script_error(res$result)) {
        return(list(
          id = tool_call$id,
          name = tool_call$name,
          result = paste0("[RECOVERY: execute_r_code]\n", res$result),
          is_error = FALSE
        ))
      }
    }
  }

  NULL
}

run_tool_safely <- function(tool_obj, args, envir = NULL) {
  tryCatch({
    result <- tool_obj$run(args, envir = envir)
    if (!is.character(result)) {
      result <- safe_to_json(result, auto_unbox = TRUE)
    }
    list(result = result, is_error = FALSE)
  }, error = function(e) {
    list(result = paste0("Error executing tool '", tool_obj$name, "': ", conditionMessage(e)),
         is_error = TRUE)
  })
}

build_skill_arg_variants <- function(parsed_args) {
  variants <- list()

  skill_name <- parsed_args$skill_name
  script_name <- parsed_args$script_name
  script_args <- parsed_args$args
  if (!is.list(script_args)) script_args <- list()

  # Variant 1: normalize dataset/data and df_name
  v1 <- parsed_args
  v1$args <- script_args
  if (is.null(v1$args$data) && !is.null(v1$args$dataset)) {
    v1$args$data <- v1$args$dataset
  }
  if (is.null(v1$args$dataset) && !is.null(v1$args$data)) {
    v1$args$dataset <- v1$args$data
  }
  if (identical(script_name, "summary.R")) {
    if (is.null(v1$args$df_name) && !is.null(v1$args$dataset)) {
      v1$args$df_name <- v1$args$dataset
    }
    if (!is.null(v1$args$df_name) && grepl("%>%|\\(|\\)", v1$args$df_name)) {
      v1$args$df_name <- strsplit(v1$args$df_name, "\\s|%>%")[[1]][1]
    }
  }
  variants <- c(variants, list(v1))

  # Variant 2: downgrade plot_scatter -> plot_histogram if x is available
  if (identical(script_name, "plot_scatter.R")) {
    v2 <- parsed_args
    v2$script_name <- "plot_histogram.R"
    v2$args <- v1$args
    if (is.null(v2$args$x) && !is.null(v2$args$y)) {
      v2$args$x <- v2$args$y
    }
    variants <- c(variants, list(v2))
  }

  # Variant 3: summary -> analyze
  if (identical(script_name, "summary.R")) {
    v3 <- parsed_args
    v3$script_name <- "analyze.R"
    v3$args <- v1$args
    if (is.null(v3$args$dataset) && !is.null(v3$args$df_name)) {
      v3$args$dataset <- v3$args$df_name
    }
    variants <- c(variants, list(v3))
  }

  variants
}

build_skill_fallback_code <- function(parsed_args) {
  script_name <- parsed_args$script_name
  script_args <- parsed_args$args
  if (!is.list(script_args)) script_args <- list()

  sanitize_name <- function(x, fallback = NULL) {
    if (is.null(x) || !nzchar(x)) return(fallback)
    if (!grepl("^[A-Za-z0-9_.]+$", x)) return(fallback)
    x
  }

  dataset <- script_args$data %||% script_args$dataset %||% script_args$df_name
  dataset <- sanitize_name(dataset, "iris")

  if (identical(script_name, "plot_scatter.R")) {
    x <- sanitize_name(script_args$x, "")
    y <- sanitize_name(script_args$y, "")
    color <- sanitize_name(script_args$color, "")
    code <- paste(
      "library(ggplot2)",
      sprintf("if (!exists('%s', inherits = TRUE)) { data(%s) }", dataset, dataset),
      sprintf("df <- get('%s', inherits = TRUE)", dataset),
      "num_cols <- names(df)[sapply(df, is.numeric)]",
      sprintf("x_col <- if (nzchar('%s')) '%s' else if (length(num_cols) >= 1) num_cols[1] else names(df)[1]", x, x),
      sprintf("y_col <- if (nzchar('%s')) '%s' else if (length(num_cols) >= 2) num_cols[2] else names(df)[1]", y, y),
      sprintf("col_col <- if (nzchar('%s')) '%s' else if ('Species' %%in%% names(df)) 'Species' else NULL", color, color),
      "p <- ggplot(df, aes_string(x = x_col, y = y_col, color = col_col)) + geom_point() + theme_minimal()",
      "p",
      sep = "\n"
    )
    return(list(code = code, description = "Fallback scatter plot"))
  }

  if (identical(script_name, "plot_histogram.R")) {
    x <- sanitize_name(script_args$x, "")
    code <- paste(
      "library(ggplot2)",
      sprintf("if (!exists('%s', inherits = TRUE)) { data(%s) }", dataset, dataset),
      sprintf("df <- get('%s', inherits = TRUE)", dataset),
      "num_cols <- names(df)[sapply(df, is.numeric)]",
      sprintf("x_col <- if (nzchar('%s')) '%s' else if (length(num_cols) >= 1) num_cols[1] else names(df)[1]", x, x),
      "p <- ggplot(df, aes_string(x = x_col)) + geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) + theme_minimal()",
      "p",
      sep = "\n"
    )
    return(list(code = code, description = "Fallback histogram"))
  }

  if (identical(script_name, "summary.R") || identical(script_name, "analyze.R")) {
    code <- paste(
      sprintf("if (!exists('%s', inherits = TRUE)) { data(%s) }", dataset, dataset),
      sprintf("df <- get('%s', inherits = TRUE)", dataset),
      "summary(df)",
      sep = "\n"
    )
    return(list(code = code, description = "Fallback summary"))
  }

  NULL
}
