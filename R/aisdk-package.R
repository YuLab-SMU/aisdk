#' @title aisdk: AI SDK for R
#' @description
#' A production-grade AI SDK for R featuring a layered architecture,
#' middleware support, robust error handling, and support for multiple
#' AI model providers.
#'
#' @section Architecture:
#' The SDK uses a 4-layer architecture:
#' \itemize{
#'   \item \strong{Specification Layer}: Abstract interfaces (LanguageModelV1, EmbeddingModelV1)
#'   \item \strong{Utilities Layer}: Shared tools (HTTP, retry, registry, middleware)
#'   \item \strong{Provider Layer}: Concrete implementations (OpenAIProvider, etc.)
#'   \item \strong{Core Layer}: High-level API (generate_text, stream_text, embed)
#' }
#'
#' @section Quick Start:
#' \preformatted{
#' library(aisdk)
#'
#' # Create an OpenAI provider
#' openai <- create_openai()
#'
#' # Generate text
#' result <- generate_text(
#'   model = openai$language_model("gpt-4o"),
#'   prompt = "Explain R in one sentence."
#' )
#' print(result$text)
#'
#' # Or use the registry for cleaner syntax
#' get_default_registry()$register("openai", openai)
#' result <- generate_text("openai:gpt-4o", "Hello!")
#' }
#'
#' @docType package
#' @name aisdk-package
"_PACKAGE"

# Package-level imports
#' @import R6
#' @importFrom rlang abort warn %||%
#' @importFrom httr2 request req_headers req_body_json req_perform req_perform_stream resp_status resp_body_json resp_body_string resp_header
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils head tail capture.output str flush.console
#' @importFrom yaml yaml.load
#' @importFrom callr r
#' @importFrom processx process
#' @importFrom digest digest
NULL
