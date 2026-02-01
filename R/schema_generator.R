#' @title Schema Generator
#' @description
#' Utilities to automatically generate z_schema objects from R function signatures.
#' @name schema_generator
NULL

#' @importFrom tools Rd_db Rd2txt
get_param_docs <- function(func_name, package = NULL) {
  if (is.null(package)) {
      # Try to find which package the function belongs to
      if (exists(func_name, mode="function")) {
          env <- environment(get(func_name))
          if (isNamespace(env)) {
              package <- getNamespaceName(env)
          }
      }
  }
  
  if (is.null(package)) return(NULL)
  
  # tools::Rd_db returns a list of all parsed Rd objects for the package
  db <- tryCatch(tools::Rd_db(package), error = function(e) NULL)
  if (is.null(db)) return(NULL)
  
  # Find the topic
  rd <- db[[paste0(func_name, ".Rd")]]
  
  # Helper to get tags
  get_tags <- function(rd) {
      sapply(rd, attr, "Rd_tag")
  }
  
  if (is.null(rd)) {
      # Fallback: Search aliases
      for (rd_file in db) {
          tags <- get_tags(rd_file)
          aliases <- rd_file[tags == "\\alias"]
          for (alias in aliases) {
              txt <- paste(capture.output(tools::Rd2txt(alias, fragment=TRUE)), collapse="")
              txt <- trimws(txt)
              if (txt == func_name) {
                  rd <- rd_file
                  break
              }
          }
          if (!is.null(rd)) break
      }
  }
  
  if (is.null(rd)) return(NULL)
  
  # Find \arguments section
  find_section <- function(rd, tag) {
     tags <- get_tags(rd)
     match <- which(tags == tag)
     if (length(match) > 0) return(rd[[match[1]]])
     NULL
  }
  
  args_section <- find_section(rd, "\\arguments")
  if (is.null(args_section)) return(NULL)
  
  docs <- list()
  
  clean_text <- function(rd_obj) {
      # Rd2txt prints to stdout, so we capture it
      txt_lines <- capture.output(tools::Rd2txt(rd_obj, fragment=TRUE))
      txt <- paste(txt_lines, collapse = " ")
      txt <- gsub("\\s+", " ", txt)
      trimws(txt)
  }
  
  for (i in seq_along(args_section)) {
      item <- args_section[[i]]
      if (!is.null(attr(item, "Rd_tag")) && attr(item, "Rd_tag") == "\\item") {
          param_names_rd <- item[[1]]
          description_rd <- item[[2]]
          
          param_names_txt <- clean_text(param_names_rd)
          description_txt <- clean_text(description_rd)
          
          params <- trimws(strsplit(param_names_txt, ",\\s*")[[1]])
          
          for (p in params) {
              docs[[p]] <- description_txt
          }
      }
  }
  return(docs)
}

#' @title Create Schema from Function
#' @description
#' Inspects an R function and generates a z_object schema based on its arguments
#' and default values.
#' @param func The R function to inspect.
#' @param include_args Optional character vector of argument names to include.
#'   If provided, only these arguments will be included in the schema.
#' @param exclude_args Optional character vector of argument names to exclude.
#' @param params Optional named list of parameter values to use as defaults.
#'   This allows overriding the function's default values (e.g., with values
#'   extracted from an existing plot layer).
#' @param func_name Optional string of the function name to look up documentation.
#'   If not provided, attempts to infer from 'func' symbol.
#' @param type_mode How to assign parameter types. "infer" (default) uses
#'   default values to infer types. "any" uses z_any() for all parameters.
#' @return A z_object schema.
#' @export
#' @examples
#' \dontrun{
#' my_func <- function(a = 1, b = "text", c = TRUE) {}
#' schema <- create_schema_from_func(my_func)
#' print(schema)
#' 
#' # Override defaults
#' schema_override <- create_schema_from_func(my_func, params = list(a = 99))
#' }
create_schema_from_func <- function(func, include_args = NULL, exclude_args = NULL,
                                    params = NULL, func_name = NULL,
                                    type_mode = c("infer", "any")) {
  if (!is.function(func)) {
    if (inherits(func, "gg") || inherits(func, "ggplot")) {
      rlang::abort(c(
        "Input must be a function, not a plot object.",
        i = "Did you mean to pass the function name (e.g., `geom_tiplab`) instead of the result (`p`)?"
      ))
    }
    rlang::abort("Input must be a function")
  }

  type_mode <- match.arg(type_mode)

  args <- as.list(formals(func))
  arg_names <- names(args)

  # Filter arguments
  if (!is.null(include_args)) {
    arg_names <- intersect(arg_names, include_args)
  }
  if (!is.null(exclude_args)) {
    arg_names <- setdiff(arg_names, exclude_args)
  }
  
  # Remove '...' if present as we can't type it easily
  arg_names <- setdiff(arg_names, "...")
  
  # Add parameter names found in 'params' (overrides/extras) that might be passed via '...'
  if (!is.null(params)) {
      extra_params <- setdiff(names(params), arg_names)
      if (length(extra_params) > 0) {
          arg_names <- c(arg_names, extra_params)
      }
  }

  # Fetch documentation if available
  doc_map <- NULL
  
  if (is.null(func_name)) {
    # Try to infer
    guess <- as.character(substitute(func))
    if (length(guess) == 1 && guess != "func") {
         func_name <- guess
    }
  }
  
  if (!is.null(func_name)) {
      doc_map <- get_param_docs(func_name)
  }

  schema_props <- list()
  required_args <- character(0)

  for (name in arg_names) {
    
    # Check if argument is missing (no default value)
    # Using as.list(formals) ensures safe access to the empty symbol
    # CRITICAL: Do not assign args[[name]] to a variable before checking,
    # as assigning the empty symbol makes the variable behave like a missing argument.
    
    # Accessing args[[name]] when name is NOT in args (extra param) returns NULL.
    # So we need to be careful.
    
    is_in_formals <- name %in% names(args)
    is_missing <- FALSE
    
    if (is_in_formals) {
        # Check directly without assignment
        is_missing <- is.symbol(args[[name]]) && as.character(args[[name]]) == ""
    }
    
    # BUT, if we have a value in params, it is NOT missing anymore
    has_param_override <- !is.null(params) && name %in% names(params)
    
    # Prepare details
    desc <- paste("Parameter", name)
    if (!is.null(doc_map) && name %in% names(doc_map)) {
        desc <- doc_map[[name]]
    }
    
    if (is_missing && !has_param_override) {
      # No default value and no override -> Required
      if (type_mode == "any") {
        schema_props[[name]] <- z_any(description = desc)
      } else {
        # For now, default to string if we can't infer type
        schema_props[[name]] <- z_string(description = desc)
      }
      required_args <- c(required_args, name)
    } else {
      # Determine the default value to use
      val <- NULL
      if (has_param_override) {
        val <- params[[name]]
      } else if (is_in_formals) {
         default_val <- args[[name]]
         val <- tryCatch(eval(default_val), error = function(e) NULL)
      }

      if (type_mode == "any") {
        schema_props[[name]] <- z_any(
          description = desc,
          default = if (!is.null(val) && length(val) == 1) val else NULL,
          nullable = is.null(val)
        )
      } else if (is.null(val)) {
        # Default is NULL, use nullable string
        schema_props[[name]] <- z_string(description = desc, nullable = TRUE, default = NULL)
      } else if (is.numeric(val)) {
        if (length(val) > 1) {
           schema_props[[name]] <- z_array(z_number(), description = desc)
        } else {
           schema_props[[name]] <- z_number(description = desc, default = val) 
        }
      } else if (is.character(val)) {
         if (length(val) > 1) {
            # Could be an enum-like vector, but in function signature `x = c("a", "b")` 
            # often means "match.arg" logic (enum default).
            # We'll treat it as an enum if it looks like one.
            # Default value is typically the first one for match.arg
            schema_props[[name]] <- z_enum(val, description = desc, default = val[1])
        } else {
            schema_props[[name]] <- z_string(description = desc, default = val)
        }
      } else if (is.logical(val)) {
        schema_props[[name]] <- z_boolean(description = desc, default = val)
      } else {
        # Fallback for complex types (lists, objects)
        schema_props[[name]] <- z_string(description = paste(desc, "(complex type)"))
      }
    }
  }

  do.call(z_object, c(schema_props, list(.required = required_args)))
}

#' @title Create Schema for ggtree Function
#' @description
#' Specialized wrapper around create_schema_from_func for ggtree/ggplot2 functions.
#' Handles common mapping and data arguments specifically.
#' @param func The R function (e.g., geom_tiplab).
#' @param layer Optional ggplot2 Layer object. If provided, its parameters
#'   (aes_params and geom_params) will be used to override the schema defaults.
#'   This is useful for creating "Edit Mode" forms for existing plot layers.
#' @return A z_object schema.
#' @export
create_z_ggtree <- function(func, layer = NULL) {
  # We typically exclude 'mapping', 'data', '...' for the simple UI layer
  # or handle them separately. For now let's exclude '...' and keep others.
  
  # Capture the function name from the call
  func_name <- as.character(substitute(func))
  if (length(func_name) != 1) func_name <- NULL
  
  params <- NULL
  if (!is.null(layer)) {
      # Extract params from the layer
      # Prioritize aes_params (user set vals) over geom_params
      
      # Extract mappings (aes)
      # Mappings are quosures/expressions. Convert to character for display.
      mappings <- NULL
      if (!is.null(layer$mapping)) {
        mappings <- lapply(layer$mapping, rlang::as_label)
      }
      
      # Merge all params:
      # 1. aes_params (fixed values)
      # 2. mappings (mapped variables)
      # 3. geom_params (internal defaults)
      # We prioritize fixed values and mappings over internal defaults.
      
      params <- c(layer$aes_params, mappings, layer$geom_params)
  }
  
  schema <- create_schema_from_func(
      func, 
      exclude_args = c("...", "mapping", "data"),
      params = params,
      func_name = func_name
  )
  
  # TODO: Add specific descriptions/enhancements for known ggtree params
  # e.g., lookup parameter docs if available
  
  schema
}
