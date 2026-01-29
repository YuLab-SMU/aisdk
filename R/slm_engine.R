#' @title Native SLM (Small Language Model) Engine
#' @description
#' Generic interface for loading and running local language models without
#' external API dependencies. Supports multiple backends including ONNX Runtime
#' and LibTorch for quantized model execution.
#' @importFrom parallel detectCores
#' @name slm_engine
NULL

#' @title SLM Engine Class
#' @description
#' R6 class for managing local Small Language Model inference.
#' Provides a unified interface for loading model weights, running inference,
#' and managing model lifecycle.
#' @export
SlmEngine <- R6::R6Class(
  "SlmEngine",

  public = list(
    #' @field model_path Path to the model weights file.
    model_path = NULL,

    #' @field model_name Human-readable model name.
    model_name = NULL,

    #' @field backend The inference backend ("onnx", "torch", "gguf").
    backend = NULL,

    #' @field config Model configuration parameters.
    config = NULL,

    #' @field loaded Whether the model is currently loaded in memory.
    loaded = FALSE,

    #' @description
    #' Create a new SLM Engine instance.
    #' @param model_path Path to the model weights file (GGUF, ONNX, or PT format).
    #' @param backend Inference backend to use: "gguf" (default), "onnx", or "torch".
    #' @param config Optional list of configuration parameters.
    #' @return A new SlmEngine object.
    initialize = function(model_path, backend = "gguf", config = list()) {
      if (!file.exists(model_path)) {
        rlang::abort(paste0("Model file not found: ", model_path))
      }

      self$model_path <- normalizePath(model_path, mustWork = TRUE)
      self$model_name <- basename(model_path)
      self$backend <- match.arg(backend, c("gguf", "onnx", "torch"))
      self$config <- private$default_config(config)

      # Validate backend availability
      private$check_backend()

      invisible(self)
    },

    #' @description
    #' Load the model into memory.
    #' @return Self (invisibly).
    load = function() {
      if (self$loaded) {
        message("Model already loaded.")
        return(invisible(self))
      }

      private$load_model()
      self$loaded <- TRUE
      message(paste0("Model loaded: ", self$model_name))

      invisible(self)
    },

    #' @description
    #' Unload the model from memory.
    #' @return Self (invisibly).
    unload = function() {
      if (!self$loaded) {
        return(invisible(self))
      }

      private$unload_model()
      self$loaded <- FALSE
      message("Model unloaded.")

      invisible(self)
    },

    #' @description
    #' Generate text completion from a prompt.
    #' @param prompt The input prompt text.
    #' @param max_tokens Maximum number of tokens to generate.
    #' @param temperature Sampling temperature (0.0 to 2.0).
    #' @param top_p Nucleus sampling parameter.
    #' @param stop Optional stop sequences.
    #' @return A list with generated text and metadata.
    generate = function(prompt,
                        max_tokens = 256,
                        temperature = 0.7,
                        top_p = 0.9,
                        stop = NULL) {
      if (!self$loaded) {
        self$load()
      }

      start_time <- Sys.time()

      result <- private$run_inference(
        prompt = prompt,
        max_tokens = max_tokens,
        temperature = temperature,
        top_p = top_p,
        stop = stop
      )

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      tokens_generated <- result$tokens_generated %||% nchar(result$text) / 4

      list(
        text = result$text,
        model = self$model_name,
        backend = self$backend,
        tokens_generated = tokens_generated,
        tokens_per_second = tokens_generated / elapsed,
        elapsed_seconds = elapsed
      )
    },

    #' @description
    #' Stream text generation with a callback function.
    #' @param prompt The input prompt text.
    #' @param callback Function called with each generated token.
    #' @param max_tokens Maximum number of tokens to generate.
    #' @param temperature Sampling temperature.
    #' @param top_p Nucleus sampling parameter.
    #' @param stop Optional stop sequences.
    #' @return A list with the complete generated text and metadata.
    stream = function(prompt,
                      callback,
                      max_tokens = 256,
                      temperature = 0.7,
                      top_p = 0.9,
                      stop = NULL) {
      if (!self$loaded) {
        self$load()
      }

      if (!is.function(callback)) {
        rlang::abort("callback must be a function")
      }

      private$run_streaming_inference(
        prompt = prompt,
        callback = callback,
        max_tokens = max_tokens,
        temperature = temperature,
        top_p = top_p,
        stop = stop
      )
    },

    #' @description
    #' Get model information and statistics.
    #' @return A list with model metadata.
    info = function() {
      list(
        model_name = self$model_name,
        model_path = self$model_path,
        backend = self$backend,
        loaded = self$loaded,
        config = self$config,
        memory_usage = if (self$loaded) private$get_memory_usage() else NA
      )
    },

    #' @description
    #' Print method for SlmEngine.
    print = function() {
      cat("<SlmEngine>\n")
      cat("  Model:", self$model_name, "\n")
      cat("  Backend:", self$backend, "\n")
      cat("  Loaded:", self$loaded, "\n")
      cat("  Path:", self$model_path, "\n")
      invisible(self)
    }
  ),

  private = list(
    .model_handle = NULL,
    .tokenizer = NULL,

    default_config = function(user_config) {
      defaults <- list(
        context_length = 2048,
        n_threads = parallel::detectCores() - 1,
        n_gpu_layers = 0,
        use_mmap = TRUE,
        use_mlock = FALSE,
        seed = -1,
        batch_size = 512
      )
      utils::modifyList(defaults, user_config)
    },

    check_backend = function() {
      # Check if required packages are available for the backend
      switch(self$backend,
        "gguf" = {
          # GGUF backend uses external llama.cpp via system calls
          # or future R bindings
          TRUE
        },
        "onnx" = {
          if (!requireNamespace("onnx", quietly = TRUE)) {
            rlang::warn("ONNX backend requires the 'onnx' package. Install with: install.packages('onnx')")
          }
        },
        "torch" = {
          if (!requireNamespace("torch", quietly = TRUE)) {
            rlang::warn("Torch backend requires the 'torch' package. Install with: install.packages('torch')")
          }
        }
      )
    },

    load_model = function() {
      switch(self$backend,
        "gguf" = private$load_gguf(),
        "onnx" = private$load_onnx(),
        "torch" = private$load_torch()
      )
    },

    unload_model = function() {
      private$.model_handle <- NULL
      private$.tokenizer <- NULL
      gc()
    },

    load_gguf = function() {
      # GGUF loading implementation
      # This will integrate with llama.cpp bindings when available
      # For now, store path for external process execution
      private$.model_handle <- list(
        type = "gguf",
        path = self$model_path,
        config = self$config
      )
    },

    load_onnx = function() {
      if (!requireNamespace("onnx", quietly = TRUE)) {
        rlang::abort("ONNX backend requires the 'onnx' package")
      }
      # ONNX loading implementation placeholder
      private$.model_handle <- list(
        type = "onnx",
        path = self$model_path
      )
    },

    load_torch = function() {
      if (!requireNamespace("torch", quietly = TRUE)) {
        rlang::abort("Torch backend requires the 'torch' package")
      }
      # LibTorch loading implementation placeholder
      private$.model_handle <- list(
        type = "torch",
        path = self$model_path
      )
    },

    run_inference = function(prompt, max_tokens, temperature, top_p, stop) {
      switch(self$backend,
        "gguf" = private$infer_gguf(prompt, max_tokens, temperature, top_p, stop),
        "onnx" = private$infer_onnx(prompt, max_tokens, temperature, top_p, stop),
        "torch" = private$infer_torch(prompt, max_tokens, temperature, top_p, stop)
      )
    },

    infer_gguf = function(prompt, max_tokens, temperature, top_p, stop) {
      # GGUF inference via llama.cpp
      # This is a placeholder that will be replaced with actual bindings
      # For now, we use a subprocess approach if llama-cli is available

      llama_cli <- Sys.which("llama-cli")
      if (nchar(llama_cli) == 0) {
        llama_cli <- Sys.which("main")  # older llama.cpp naming
      }

      if (nchar(llama_cli) == 0) {
        rlang::abort(
          "GGUF inference requires llama.cpp. Install from: https://github.com/ggerganov/llama.cpp",
          class = "slm_backend_missing"
        )
      }

      # Build command arguments
      args <- c(
        "-m", self$model_path,
        "-p", prompt,
        "-n", as.character(max_tokens),
        "--temp", as.character(temperature),
        "--top-p", as.character(top_p),
        "-t", as.character(self$config$n_threads),
        "-c", as.character(self$config$context_length),
        "--no-display-prompt"
      )

      if (!is.null(stop)) {
        for (s in stop) {
          args <- c(args, "-s", s)
        }
      }

      # Run inference
      result <- tryCatch({
        processx::run(llama_cli, args, timeout = 300)
      }, error = function(e) {
        rlang::abort(paste0("GGUF inference failed: ", conditionMessage(e)))
      })

      list(
        text = trimws(result$stdout),
        tokens_generated = max_tokens  # Approximate
      )
    },

    infer_onnx = function(prompt, max_tokens, temperature, top_p, stop) {
      # ONNX Runtime inference placeholder
      rlang::abort("ONNX inference not yet implemented")
    },

    infer_torch = function(prompt, max_tokens, temperature, top_p, stop) {
      # LibTorch inference placeholder
      rlang::abort("Torch inference not yet implemented")
    },

    run_streaming_inference = function(prompt, callback, max_tokens, temperature, top_p, stop) {
      # Streaming inference - backend specific
      # For GGUF, we can use processx with stdout callback

      if (self$backend != "gguf") {
        rlang::abort("Streaming only supported for GGUF backend currently")
      }

      llama_cli <- Sys.which("llama-cli")
      if (nchar(llama_cli) == 0) {
        llama_cli <- Sys.which("main")
      }

      if (nchar(llama_cli) == 0) {
        rlang::abort("GGUF streaming requires llama.cpp")
      }

      args <- c(
        "-m", self$model_path,
        "-p", prompt,
        "-n", as.character(max_tokens),
        "--temp", as.character(temperature),
        "--top-p", as.character(top_p),
        "-t", as.character(self$config$n_threads),
        "-c", as.character(self$config$context_length),
        "--no-display-prompt"
      )

      collected_text <- ""

      process <- processx::process$new(
        llama_cli,
        args,
        stdout = "|",
        stderr = "|"
      )

      while (process$is_alive()) {
        chunk <- process$read_output(n = 100)
        if (nchar(chunk) > 0) {
          collected_text <- paste0(collected_text, chunk)
          callback(chunk)
        }
        Sys.sleep(0.01)
      }

      # Get any remaining output
      final_chunk <- process$read_all_output()
      if (nchar(final_chunk) > 0) {
        collected_text <- paste0(collected_text, final_chunk)
        callback(final_chunk)
      }

      list(
        text = collected_text,
        tokens_generated = max_tokens
      )
    },

    get_memory_usage = function() {
      # Estimate memory usage based on model file size
      file.info(self$model_path)$size / (1024^3)  # GB
    }
  )
)

#' @title Create SLM Engine
#' @description
#' Factory function to create a new SLM Engine for local model inference.
#' @param model_path Path to the model weights file.
#' @param backend Inference backend: "gguf" (default), "onnx", or "torch".
#' @param config Optional configuration list.
#' @return An SlmEngine object.
#' @export
#' @examples
#' \dontrun{
#' # Load a GGUF model
#' engine <- slm_engine("models/llama-3-8b-q4.gguf")
#' engine$load()
#'
#' # Generate text
#' result <- engine$generate("What is the capital of France?")
#' cat(result$text)
#'
#' # Stream generation
#' engine$stream("Tell me a story", callback = cat)
#'
#' # Cleanup
#' engine$unload()
#' }
slm_engine <- function(model_path, backend = "gguf", config = list()) {
  SlmEngine$new(model_path, backend, config)
}

#' @title List Available Local Models
#' @description
#' Scan common directories for available local model files.
#' @param paths Character vector of directories to scan. Defaults to common locations.
#' @return A data frame with model information.
#' @export
list_local_models <- function(paths = NULL) {
  if (is.null(paths)) {
    paths <- c(
      file.path(Sys.getenv("HOME"), ".cache", "huggingface"),
      file.path(Sys.getenv("HOME"), "models"),
      file.path(Sys.getenv("HOME"), ".ollama", "models"),
      "models",
      "."
    )
  }

  # Filter to existing directories
  paths <- paths[dir.exists(paths)]

  if (length(paths) == 0) {
    return(data.frame(
      name = character(),
      path = character(),
      size_gb = numeric(),
      format = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Find model files
  patterns <- c("\\.gguf$", "\\.onnx$", "\\.pt$", "\\.bin$")

  files <- unlist(lapply(paths, function(p) {
    list.files(p, pattern = paste(patterns, collapse = "|"),
               recursive = TRUE, full.names = TRUE)
  }))

  if (length(files) == 0) {
    return(data.frame(
      name = character(),
      path = character(),
      size_gb = numeric(),
      format = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build result data frame
  info <- file.info(files)

  data.frame(
    name = basename(files),
    path = files,
    size_gb = round(info$size / (1024^3), 2),
    format = tools::file_ext(files),
    stringsAsFactors = FALSE
  )
}

#' @title Download Model from Hugging Face
#' @description
#' Download a quantized model from Hugging Face Hub.
#' @param repo_id The Hugging Face repository ID (e.g., "TheBloke/Llama-2-7B-GGUF").
#' @param filename The specific file to download.
#' @param dest_dir Destination directory. Defaults to "~/.cache/aisdk/models".
#' @param quiet Suppress download progress.
#' @return Path to the downloaded file.
#' @export
download_model <- function(repo_id, filename, dest_dir = NULL, quiet = FALSE) {
  if (is.null(dest_dir)) {
    dest_dir <- file.path(Sys.getenv("HOME"), ".cache", "aisdk", "models")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_path <- file.path(dest_dir, filename)

  if (file.exists(dest_path)) {
    if (!quiet) message("Model already exists: ", dest_path)
    return(dest_path)
  }

  # Construct Hugging Face URL
  url <- paste0(
    "https://huggingface.co/",
    repo_id,
    "/resolve/main/",
    filename
  )

  if (!quiet) message("Downloading: ", url)

  # Download with progress
  tryCatch({
    utils::download.file(
      url,
      dest_path,
      mode = "wb",
      quiet = quiet
    )
    dest_path
  }, error = function(e) {
    if (file.exists(dest_path)) file.remove(dest_path)
    rlang::abort(paste0("Download failed: ", conditionMessage(e)))
  })
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
