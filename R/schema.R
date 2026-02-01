#' @title Schema DSL: Lightweight JSON Schema Generator
#' @description
#' A lightweight DSL (Domain Specific Language) for defining JSON Schema structures
#' in R, inspired by Zod from TypeScript. Used for defining tool parameters.
#' @name schema
NULL

# ============================================================================
# Primitive Type Builders
# ============================================================================

#' @title Create String Schema
#' @description Create a JSON Schema for string type.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @return A list representing JSON Schema for string.
#' @export
#' @examples
#' z_string(description = "The city name")
z_string <- function(description = NULL, nullable = FALSE, default = NULL) {
  schema <- list(type = "string")
  if (!is.null(description)) {
    schema$description <- description
  }
  if (!is.null(default)) {
    schema$default <- default
  }
  if (nullable) {
    schema$type <- c("string", "null")
  }
  class(schema) <- c("z_schema", "z_string", "list")
  schema
}

#' @title Create Number Schema
#' @description Create a JSON Schema for number (floating point) type.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @param minimum Optional minimum value.
#' @param maximum Optional maximum value.
#' @return A list representing JSON Schema for number.
#' @export
#' @examples
#' z_number(description = "Temperature value", minimum = -100, maximum = 100)
z_number <- function(description = NULL, nullable = FALSE, default = NULL,
                     minimum = NULL, maximum = NULL) {
  schema <- list(type = "number")
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (nullable) schema$type <- c("number", "null")
  if (!is.null(minimum)) schema$minimum <- minimum
  if (!is.null(maximum)) schema$maximum <- maximum
  class(schema) <- c("z_schema", "z_number", "list")
  schema
}

#' @title Create Integer Schema
#' @description Create a JSON Schema for integer type.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @param minimum Optional minimum value.
#' @param maximum Optional maximum value.
#' @return A list representing JSON Schema for integer.
#' @export
#' @examples
#' z_integer(description = "Number of items", minimum = 0)
z_integer <- function(description = NULL, nullable = FALSE, default = NULL,
                      minimum = NULL, maximum = NULL) {
  schema <- list(type = "integer")
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (nullable) schema$type <- c("integer", "null")
  if (!is.null(minimum)) schema$minimum <- minimum
  if (!is.null(maximum)) schema$maximum <- maximum
  class(schema) <- c("z_schema", "z_integer", "list")
  schema
}

#' @title Create Boolean Schema
#' @description Create a JSON Schema for boolean type.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @return A list representing JSON Schema for boolean.
#' @export
#' @examples
#' z_boolean(description = "Whether to include details")
z_boolean <- function(description = NULL, nullable = FALSE, default = NULL) {
  schema <- list(type = "boolean")
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (nullable) schema$type <- c("boolean", "null")
  class(schema) <- c("z_schema", "z_boolean", "list")
  schema
}

#' @title Create Any Schema
#' @description Create a JSON Schema that accepts any JSON value.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @return A list representing JSON Schema for any value.
#' @export
#' @examples
#' z_any(description = "Flexible input")
z_any <- function(description = NULL, nullable = TRUE, default = NULL) {
  schema <- list(type = c("string", "number", "integer", "boolean", "object", "array", "null"))
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (!nullable) schema$type <- setdiff(schema$type, "null")
  class(schema) <- c("z_schema", "z_any", "list")
  schema
}

# ============================================================================
# Complex Type Builders
# ============================================================================

#' @title Create Enum Schema
#' @description Create a JSON Schema for string enum type.
#' @param values Character vector of allowed values.
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @return A list representing JSON Schema for enum.
#' @export
#' @examples
#' z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
z_enum <- function(values, description = NULL, nullable = FALSE, default = NULL) {
  if (!is.character(values) || length(values) == 0) {
    rlang::abort("z_enum requires a non-empty character vector of values")
  }
  schema <- list(
    type = "string",
    enum = as.list(values)  # Use list to preserve as array in JSON
  )
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (nullable) schema$type <- c("string", "null")
  class(schema) <- c("z_schema", "z_enum", "list")
  schema
}

#' @title Create Array Schema
#' @description Create a JSON Schema for array type.
#' @param items Schema for array items (created by z_* functions).
#' @param description Optional description of the field.
#' @param nullable If TRUE, allows null values.
#' @param default Optional default value.
#' @param min_items Optional minimum number of items.
#' @param max_items Optional maximum number of items.
#' @return A list representing JSON Schema for array.
#' @export
#' @examples
#' z_array(z_string(), description = "List of names")
z_array <- function(items, description = NULL, nullable = FALSE, default = NULL,
                    min_items = NULL, max_items = NULL) {
  if (!inherits(items, "z_schema")) {
    rlang::abort("z_array 'items' must be a z_schema object (created by z_* functions)")
  }
  schema <- list(
    type = "array",
    items = items
  )
  if (!is.null(description)) schema$description <- description
  if (!is.null(default)) schema$default <- default
  if (nullable) schema$type <- c("array", "null")
  if (!is.null(min_items)) schema$minItems <- min_items
  if (!is.null(max_items)) schema$maxItems <- max_items
  class(schema) <- c("z_schema", "z_array", "list")
  schema
}

#' @title Create Object Schema
#' @description
#' Create a JSON Schema for object type. This is the primary schema builder
#' for defining tool parameters.
#' @param ... Named arguments where names are property names and values are
#'   z_schema objects created by z_* functions.
#' @param .description Optional description of the object.
#' @param .required Character vector of required field names. If NULL (default),
#'   all fields are considered required.
#' @param .additional_properties Whether to allow additional properties. Default FALSE.
#' @return A list representing JSON Schema for object.
#' @export
#' @examples
#' z_object(
#'   location = z_string(description = "City name, e.g., Beijing"),
#'   unit = z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
#' )
z_object <- function(..., .description = NULL, .required = NULL, 
                     .additional_properties = FALSE) {
  props <- list(...)
  
  if (length(props) == 0) {
    rlang::abort("z_object requires at least one property")
  }
  
  # Check for unnamed properties
  prop_names <- names(props)
  if (is.null(prop_names) || any(prop_names == "")) {
    rlang::abort("All properties in z_object must be named")
  }
  
  # Validate all properties are z_schema objects
  for (name in prop_names) {
    if (!inherits(props[[name]], "z_schema")) {
      rlang::abort(paste0(
        "Property '", name, "' must be a z_schema object (created by z_* functions)"
      ))
    }
  }
  
  # Build required array
  required <- if (is.null(.required)) names(props) else .required
  
  schema <- list(
    type = "object",
    properties = props,
    required = as.list(required),
    additionalProperties = .additional_properties
  )
  
  if (!is.null(.description)) schema$description <- .description
  
  class(schema) <- c("z_schema", "z_object", "list")
  schema
}

z_any_object <- function(description = NULL) {
  schema <- list(
    type = "object",
    additionalProperties = TRUE
  )
  if (!is.null(description)) schema$description <- description
  class(schema) <- c("z_schema", "z_any_object", "list")
  schema
}

#' @title Create Empty Object Schema
#' @description Create a JSON Schema for an empty object `{}`.
#' @param description Optional description.
#' @return A z_schema object.
#' @export
z_empty_object <- function(description = NULL) {
  schema <- list(
    type = "object",
    properties = structure(list(), names = character(0)),
    required = list(),
    additionalProperties = FALSE
  )
  if (!is.null(description)) schema$description <- description
  class(schema) <- c("z_schema", "z_object", "list")
  schema
}


#' @title Describe Schema
#' @description Add a description to a z_schema object (pipe-friendly).
#' @param schema A z_schema object.
#' @param description The description string.
#' @return The modified z_schema object.
#' @export
z_describe <- function(schema, description) {
  if (!inherits(schema, "z_schema")) {
    rlang::abort("schema must be a z_schema object")
  }
  schema$description <- description
  schema
}

# ============================================================================
# Serialization
# ============================================================================

#' @title Convert Schema to JSON
#' @description
#' Convert a z_schema object to a JSON string suitable for API calls.
#' Handles the R-specific auto_unbox issues properly.
#' @param schema A z_schema object created by z_* functions.
#' @param pretty If TRUE, format JSON with indentation.
#' @return A JSON string.
#' @export
#' @examples
#' schema <- z_object(
#'   name = z_string(description = "User name")
#' )
#' cat(schema_to_json(schema, pretty = TRUE))
schema_to_json <- function(schema, pretty = FALSE) {
  if (!inherits(schema, "z_schema")) {
    rlang::abort("schema must be a z_schema object")
  }
  
  # Convert to plain list for JSON serialization
  plain <- schema_to_list(schema)
  
  jsonlite::toJSON(plain, auto_unbox = TRUE, pretty = pretty, null = "null")
}

#' @title Safe Serialization to JSON
#' @description
#' Standardized internal helper for JSON serialization with common defaults.
#' @param x Object to serialize.
#' @param auto_unbox Whether to automatically unbox single-element vectors. Default TRUE.
#' @param ... Additional arguments to jsonlite::toJSON.
#' @return A JSON string.
#' @export
safe_to_json <- function(x, auto_unbox = TRUE, ...) {
  if (inherits(x, "ggplot") || inherits(x, "gg")) {
    x <- ggplot_to_z_object(x, include_data = TRUE, include_render_hints = TRUE)
  }

  tryCatch(
    jsonlite::toJSON(x, auto_unbox = auto_unbox, ..., null = "null"),
    error = function(e) {
      fallback <- list(
        error = "non_serializable_result",
        class = paste(class(x), collapse = ","),
        message = conditionMessage(e),
        preview = paste(utils::capture.output(utils::str(x, max.level = 2, vec.len = 5)),
                        collapse = "\n")
      )
      jsonlite::toJSON(fallback, auto_unbox = TRUE, null = "null")
    }
  )
}



#' @title Convert Schema to Plain List
#' @description
#' Internal function to convert z_schema to plain list, stripping class attributes.
#' @param schema A z_schema object.
#' @return A plain list suitable for JSON conversion.
#' @keywords internal
schema_to_list <- function(schema) {
  if (!is.list(schema)) return(schema)
  
  # Remove z_schema classes
  result <- lapply(schema, function(x) {
    if (inherits(x, "z_schema")) {
      schema_to_list(x)
    } else if (is.list(x)) {
      lapply(x, schema_to_list)
    } else {
      x
    }
  })
  
  # Keep as list, not data.frame
  class(result) <- "list"
  result
}

# ============================================================================
# R-Specific Helpers
# ============================================================================

#' @title Create Dataframe Schema
#' @description
#' Create a schema that represents a dataframe (or list of row objects).
#' This is an R-specific convenience function that generates a JSON Schema
#' for an array of objects. The LLM will be instructed to output data in a
#' format that can be easily converted to an R dataframe using
#' `dplyr::bind_rows()` or `do.call(rbind, lapply(..., as.data.frame))`.
#'
#' @param ... Named arguments where names are column names and values are
#'   z_schema objects representing the column types.
#' @param .description Optional description of the dataframe.
#' @param .min_rows Optional minimum number of rows.
#' @param .max_rows Optional maximum number of rows.
#' @return A z_schema object representing an array of objects.
#' @export
#' @examples
#' # Define a schema for a dataframe of genes
#' gene_schema <- z_dataframe(
#'   gene_name = z_string(description = "Name of the gene"),
#'   expression = z_number(description = "Expression level"),
#'   significant = z_boolean(description = "Is statistically significant")
#' )
#'
#' # Use with generate_object
#' # result <- generate_object(model, "Extract gene data...", gene_schema)
#' # df <- dplyr::bind_rows(result$object)
#' @title Create Dataframe Schema
#' @description
#' Create a schema that represents a dataframe (or list of row objects).
#' This is an R-specific convenience function that generates a JSON Schema
#' for an array of objects.
#'
#' @param ... Named arguments where names are column names and values are
#'   z_schema objects representing the column types.
#' @param .description Optional description of the dataframe.
#' @param .nullable If TRUE, allows null values.
#' @param .default Optional default value.
#' @param .min_rows Optional minimum number of rows.
#' @param .max_rows Optional maximum number of rows.
#' @return A z_schema object representing an array of objects.
#' @export
z_dataframe <- function(..., .description = NULL, .nullable = FALSE, .default = NULL,
                        .min_rows = NULL, .max_rows = NULL) {
  # Create the row object schema
  row_schema <- z_object(...)
  
  # Wrap in array schema
  schema <- z_array(
    row_schema,
    description = .description,
    nullable = .nullable,
    default = .default,
    min_items = .min_rows,
    max_items = .max_rows
  )
  
  # Add a marker class for special handling if needed
  class(schema) <- c("z_dataframe", class(schema))
  schema
}

#' @title Print Method for z_schema
#' @description Pretty print a z_schema object.
#' @param x A z_schema object.
#' @param ... Additional arguments (ignored).
#' @export
print.z_schema <- function(x, ...) {
  cat("<z_schema>\n")
  cat(schema_to_json(x, pretty = TRUE), "\n")
  invisible(x)
}
