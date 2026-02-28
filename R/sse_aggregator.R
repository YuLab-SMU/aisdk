#' @title SSE Stream Aggregator
#' @description
#' Universal chunk aggregator for Server-Sent Events (SSE) streaming.
#' Manages all chunk-level state: text accumulation, reasoning/thinking
#' transitions, tool call assembly, usage tracking, and result finalization.
#'
#' This is a pure aggregator — it does not know about HTTP, SSE parsing,
#' or provider-specific event types. Provider event mappers call its methods.
#'
#' @name sse_aggregator
#' @keywords internal
NULL

#' @title SSEAggregator R6 Class
#' @description
#' Accumulates streaming chunks into a final GenerateResult.
#'
#' Handles two tool call formats:
#' - **OpenAI format**: Chunked deltas with index, id, function.name, function.arguments
#' - **Anthropic format**: content_block_start with id+name, then input_json_delta chunks
#'
#' @keywords internal
SSEAggregator <- R6::R6Class(
    "SSEAggregator",
    public = list(
        #' @description Initialize the aggregator.
        #' @param callback User callback function: callback(text, done).
        initialize = function(callback) {
            private$callback <- callback
            private$full_text <- ""
            private$full_reasoning <- ""
            private$is_reasoning <- FALSE
            private$tool_calls_acc <- list()
            private$tool_args_acc <- list()
            private$finish_reason <- NULL
            private$full_usage <- NULL
            private$last_response <- NULL
        },

        # ------------------------------------------------------------------
        # Content methods
        # ------------------------------------------------------------------

        #' @description Handle a text content delta.
        #' @param text The text chunk.
        on_text_delta = function(text) {
            if (is.null(text) || nchar(text) == 0) {
                return(invisible())
            }
            # Close reasoning block if transitioning
            if (private$is_reasoning) {
                private$callback("\n</think>\n\n", done = FALSE)
                private$is_reasoning <- FALSE
            }
            private$full_text <- paste0(private$full_text, text)
            private$callback(text, done = FALSE)
        },

        #' @description Handle a reasoning/thinking content delta.
        #' @param text The reasoning text chunk.
        on_reasoning_delta = function(text) {
            if (is.null(text) || nchar(text) == 0) {
                return(invisible())
            }
            if (!private$is_reasoning) {
                private$callback("<think>\n", done = FALSE)
                private$is_reasoning <- TRUE
            }
            private$full_reasoning <- paste0(private$full_reasoning, text)
            private$callback(text, done = FALSE)
        },

        #' @description Signal the start of a reasoning block (Anthropic thinking).
        on_reasoning_start = function() {
            if (!private$is_reasoning) {
                private$callback("<think>\n", done = FALSE)
                private$is_reasoning <- TRUE
            }
        },

        #' @description Signal content block stop (closes reasoning if open).
        on_block_stop = function() {
            if (private$is_reasoning) {
                private$callback("\n</think>\n\n", done = FALSE)
                private$is_reasoning <- FALSE
            }
        },

        # ------------------------------------------------------------------
        # Tool call methods — OpenAI format (chunked deltas)
        # ------------------------------------------------------------------

        #' @description Handle OpenAI-format tool call deltas.
        #' @param tool_calls List of tool call delta objects from the choices delta.
        on_tool_call_delta = function(tool_calls) {
            if (is.null(tool_calls)) {
                return(invisible())
            }
            for (tc in tool_calls) {
                idx <- if (!is.null(tc$index)) tc$index + 1 else length(private$tool_calls_acc) + 1
                private$tool_calls_acc <- private$ensure_index(
                    private$tool_calls_acc, idx,
                    list(id = "", name = "", arguments = "", arguments_is_list = FALSE)
                )

                if (!is.null(tc$id)) {
                    private$tool_calls_acc[[idx]]$id <- paste0(private$tool_calls_acc[[idx]]$id, tc$id)
                }

                # Extract name from multiple possible locations
                name_val <- NULL
                if (!is.null(tc$`function`$name)) name_val <- tc$`function`$name
                if (is.null(name_val) && !is.null(tc$name)) name_val <- tc$name
                if (is.null(name_val) && !is.null(tc$tool_name)) name_val <- tc$tool_name
                if (!is.null(name_val)) {
                    private$tool_calls_acc[[idx]]$name <- paste0(private$tool_calls_acc[[idx]]$name, name_val)
                }

                # Extract arguments from multiple possible locations
                args_val <- NULL
                if (!is.null(tc$`function`$arguments)) args_val <- tc$`function`$arguments
                if (is.null(args_val) && !is.null(tc$arguments)) args_val <- tc$arguments
                if (!is.null(args_val)) {
                    if (is.list(args_val) && !is.character(args_val)) {
                        private$tool_calls_acc[[idx]]$arguments <- args_val
                        private$tool_calls_acc[[idx]]$arguments_is_list <- TRUE
                    } else {
                        private$tool_calls_acc[[idx]]$arguments <- paste0(
                            private$tool_calls_acc[[idx]]$arguments, args_val
                        )
                    }
                }
            }
        },

        # ------------------------------------------------------------------
        # Tool call methods — Anthropic format (block start + input deltas)
        # ------------------------------------------------------------------

        #' @description Handle Anthropic-format tool use block start.
        #' @param index Block index (0-based from API, converted to 1-based internally).
        #' @param id Tool call ID.
        #' @param name Tool name.
        #' @param input Initial input (usually NULL or empty).
        on_tool_start = function(index, id, name, input = NULL) {
            idx <- index + 1
            private$tool_calls_acc <- private$ensure_index(private$tool_calls_acc, idx, NULL)
            private$tool_calls_acc[[idx]] <- list(
                id = id %||% "",
                name = name %||% "",
                arguments = input
            )
            private$tool_args_acc <- private$ensure_index(private$tool_args_acc, idx, "")
        },

        #' @description Handle Anthropic-format input_json_delta.
        #' @param index Block index (0-based from API).
        #' @param partial_json Partial JSON string.
        on_tool_input_delta = function(index, partial_json) {
            idx <- index + 1
            private$tool_args_acc <- private$ensure_index(private$tool_args_acc, idx, "")
            private$tool_args_acc[[idx]] <- paste0(private$tool_args_acc[[idx]], partial_json)
        },

        # ------------------------------------------------------------------
        # Metadata methods
        # ------------------------------------------------------------------

        #' @description Store finish reason.
        #' @param reason The finish reason string.
        on_finish_reason = function(reason) {
            if (!is.null(reason)) {
                private$finish_reason <- reason
            }
        },

        #' @description Store usage information.
        #' @param usage Usage list.
        on_usage = function(usage) {
            if (!is.null(usage)) {
                private$full_usage <- usage
            }
        },

        #' @description Store last raw response for diagnostics.
        #' @param response The raw response data.
        on_raw_response = function(response) {
            private$last_response <- response
        },

        #' @description Signal stream completion.
        on_done = function() {
            if (private$is_reasoning) {
                private$callback("\n</think>\n\n", done = FALSE)
                private$is_reasoning <- FALSE
            }
            private$callback(NULL, done = TRUE)
        },

        # ------------------------------------------------------------------
        # Finalization
        # ------------------------------------------------------------------

        #' @description Finalize accumulated state into a GenerateResult.
        #' @return A GenerateResult object.
        finalize = function() {
            final_tool_calls <- private$finalize_tool_calls()

            GenerateResult$new(
                text = private$full_text,
                usage = private$full_usage,
                finish_reason = private$finish_reason,
                raw_response = private$last_response,
                tool_calls = final_tool_calls,
                reasoning = private$full_reasoning
            )
        }
    ),
    private = list(
        callback = NULL,
        full_text = "",
        full_reasoning = "",
        is_reasoning = FALSE,
        tool_calls_acc = list(),
        tool_args_acc = list(),
        finish_reason = NULL,
        full_usage = NULL,
        last_response = NULL,
        ensure_index = function(lst, idx, default) {
            if (length(lst) < idx) {
                for (i in seq(from = length(lst) + 1, to = idx)) {
                    lst[[i]] <- default
                }
            }
            lst
        },
        finalize_tool_calls = function() {
            # Determine which format was used
            has_openai_format <- length(private$tool_calls_acc) > 0 &&
                !is.null(private$tool_calls_acc[[1]]) &&
                !is.null(private$tool_calls_acc[[1]]$arguments_is_list)

            has_anthropic_format <- length(private$tool_args_acc) > 0 &&
                any(nzchar(unlist(private$tool_args_acc)))

            if (has_openai_format) {
                return(private$finalize_openai_tool_calls())
            } else if (has_anthropic_format || length(private$tool_calls_acc) > 0) {
                return(private$finalize_anthropic_tool_calls())
            }

            NULL
        },
        finalize_openai_tool_calls = function() {
            if (length(private$tool_calls_acc) == 0) {
                return(NULL)
            }

            final <- lapply(private$tool_calls_acc, function(tc) {
                args_val <- if (isTRUE(tc$arguments_is_list)) {
                    tc$arguments
                } else {
                    parse_tool_arguments(tc$arguments, tool_name = tc$name)
                }
                list(id = tc$id, name = tc$name, arguments = args_val)
            })
            final <- Filter(function(tc) nzchar(tc$name %||% ""), final)
            if (length(final) == 0) NULL else final
        },
        finalize_anthropic_tool_calls = function() {
            all_indices <- union(seq_along(private$tool_calls_acc), seq_along(private$tool_args_acc))
            if (length(all_indices) == 0) {
                return(NULL)
            }

            final <- lapply(all_indices, function(idx) {
                tc <- if (idx <= length(private$tool_calls_acc)) private$tool_calls_acc[[idx]] else NULL
                raw_args <- if (idx <= length(private$tool_args_acc)) private$tool_args_acc[[idx]] else ""

                args_val <- if (!is.null(raw_args) && nzchar(raw_args)) {
                    parse_tool_arguments(raw_args, tool_name = tc$name %||% "unknown")
                } else if (!is.null(tc) && !is_empty_args_internal(tc$arguments)) {
                    tc$arguments
                } else {
                    list()
                }

                list(
                    id = if (!is.null(tc) && !is.null(tc$id)) tc$id else "",
                    name = if (!is.null(tc) && !is.null(tc$name)) tc$name else "",
                    arguments = args_val
                )
            })
            final <- Filter(function(tc) nzchar(tc$name %||% ""), final)
            if (length(final) == 0) NULL else final
        }
    )
)

# Internal helper — check if arguments are "empty" (NULL or empty list)
# Avoids dependency on provider_anthropic.R's is_empty_args
is_empty_args_internal <- function(args) {
    if (is.null(args)) {
        return(TRUE)
    }
    if (is.list(args) && length(args) == 0) {
        return(TRUE)
    }
    FALSE
}


# ============================================================================
# Event Mappers
# ============================================================================

#' @title Map OpenAI SSE chunk to aggregator calls
#' @description
#' Translates an OpenAI Chat Completions SSE data chunk into
#' the appropriate SSEAggregator method calls.
#' @param data Parsed JSON data from SSE event (or NULL if done).
#' @param done Logical, TRUE if stream is complete.
#' @param agg An SSEAggregator instance.
#' @keywords internal
map_openai_chunk <- function(data, done, agg) {
    if (done) {
        agg$on_done()
        return(invisible())
    }

    if (!is.null(data$choices) && length(data$choices) > 0) {
        agg$on_raw_response(data)
        delta <- data$choices[[1]]$delta
        choice <- data$choices[[1]]

        # Finish reason
        if (!is.null(choice$finish_reason)) {
            agg$on_finish_reason(choice$finish_reason)
        }

        # Reasoning content (DeepSeek / Doubao etc.)
        if (!is.null(delta$reasoning_content) && nchar(delta$reasoning_content) > 0) {
            agg$on_reasoning_delta(delta$reasoning_content)
        }

        # Text content
        if (!is.null(delta$content) && nchar(delta$content) > 0) {
            agg$on_text_delta(delta$content)
        }

        # Tool calls
        if (!is.null(delta$tool_calls)) {
            agg$on_tool_call_delta(delta$tool_calls)
        }
    }

    # Usage (may appear at top level, outside choices)
    if (!is.null(data$usage)) {
        agg$on_usage(data$usage)
    }
}


#' @title Map native Anthropic SSE event to aggregator calls
#' @description
#' Translates a native Anthropic Messages API SSE event into
#' the appropriate SSEAggregator method calls.
#' @param event_type SSE event type string (e.g. "content_block_delta").
#' @param event_data Parsed JSON data from SSE event.
#' @param agg An SSEAggregator instance.
#' @return Logical TRUE if the stream should break (message_stop received).
#' @keywords internal
map_anthropic_chunk <- function(event_type, event_data, agg) {
    agg$on_raw_response(event_data)

    if (event_type == "content_block_delta") {
        delta <- event_data$delta
        if (!is.null(delta)) {
            if (delta$type == "text_delta" && !is.null(delta$text)) {
                agg$on_text_delta(delta$text)
            } else if (delta$type == "thinking_delta" && !is.null(delta$thinking)) {
                agg$on_reasoning_delta(delta$thinking)
            } else if (delta$type == "input_json_delta" && !is.null(delta$partial_json)) {
                idx <- if (is.null(event_data$index)) 0 else event_data$index
                agg$on_tool_input_delta(idx, delta$partial_json)
            }
        }
    } else if (event_type == "content_block_start") {
        cb <- event_data$content_block
        idx <- if (is.null(event_data$index)) 0 else event_data$index

        if (!is.null(cb) && !is.null(cb$type)) {
            if (cb$type == "tool_use") {
                agg$on_tool_start(idx, cb$id, cb$name, cb$input)
            } else if (cb$type == "thinking") {
                agg$on_reasoning_start()
            }
            # "text" block start — no action needed
        }
    } else if (event_type == "content_block_stop") {
        agg$on_block_stop()
    } else if (event_type == "message_delta") {
        if (!is.null(event_data$delta$stop_reason)) {
            agg$on_finish_reason(event_data$delta$stop_reason)
        }
        if (!is.null(event_data$usage)) {
            agg$on_usage(list(
                prompt_tokens = 0,
                completion_tokens = event_data$usage$output_tokens,
                total_tokens = event_data$usage$output_tokens
            ))
        }
    } else if (event_type == "message_stop") {
        agg$on_done()
        return(TRUE) # Signal to break the loop
    }

    FALSE # Continue processing
}
