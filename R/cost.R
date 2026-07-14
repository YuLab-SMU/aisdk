#' @title Token Cost Estimation
#' @description
#' Cache-aware cost estimation for a generation from its token usage and a
#' per-model pricing table. Prices are indicative and change often, so the
#' built-in table is a starting point you can override globally with
#' `options(aisdk.model_pricing = ...)` or per model with [set_model_pricing()].
#' @name cost
#' @keywords internal
NULL

#' @keywords internal
# Indicative pricing in USD per million tokens. Change frequently — override
# rather than rely on these for billing. Cache rates, when omitted, default to
# Anthropic's published multipliers (read 0.1x input, write 1.25x input).
# Longer keys win on prefix match, so a family key ("claude-sonnet") backstops
# specific versions ("claude-sonnet-4-5-20250929").
aisdk_builtin_model_pricing <- function() {
  list(
    "gpt-4o"        = list(input = 2.50, output = 10.00, cache_read = 1.25),
    "gpt-4o-mini"   = list(input = 0.15, output = 0.60,  cache_read = 0.075),
    "o1"            = list(input = 15.00, output = 60.00),
    "o3-mini"       = list(input = 1.10, output = 4.40),
    "claude-haiku"  = list(input = 1.00, output = 5.00),
    "claude-sonnet" = list(input = 3.00, output = 15.00),
    "claude-opus"   = list(input = 5.00, output = 25.00),
    "gemini-2.5-flash" = list(input = 0.30, output = 2.50),
    "gemini-2.5-pro"   = list(input = 1.25, output = 10.00)
  )
}

#' @keywords internal
# Strip a "provider:" prefix and lowercase, so "openai:gpt-4o" resolves.
normalize_pricing_model_id <- function(model_id) {
  id <- tolower(trimws(model_id %||% ""))
  sub("^[a-z0-9_.-]+:", "", id)
}

#' Resolve the pricing entry for a model
#'
#' Looks up the user override table (`options(aisdk.model_pricing)`) first,
#' then the built-in table, matching an exact id or the longest key that is a
#' prefix of the id (so "claude-sonnet-4-5" matches "claude-sonnet").
#'
#' @param model_id A model id, optionally "provider:model".
#' @return A pricing list (`input`, `output`, optional `cache_read`/`cache_write`)
#'   or `NULL` if unknown.
#' @keywords internal
#' @export
resolve_model_pricing <- function(model_id) {
  id <- normalize_pricing_model_id(model_id)
  if (!nzchar(id)) {
    return(NULL)
  }
  lookup <- function(table) {
    if (!is.list(table) || length(table) == 0) {
      return(NULL)
    }
    # Normalize keys the same way as the query (strip any provider prefix) so a
    # user entry like "openai:gpt-4o" matches a normalized "gpt-4o" query.
    keys <- vapply(names(table), normalize_pricing_model_id, character(1), USE.NAMES = FALSE)
    if (id %in% keys) {
      return(table[[which(keys == id)[[1]]]])
    }
    prefix_keys <- keys[vapply(keys, function(k) nzchar(k) && startsWith(id, k), logical(1))]
    if (length(prefix_keys) > 0) {
      best <- prefix_keys[[which.max(nchar(prefix_keys))]]
      return(table[[which(keys == best)[[1]]]])
    }
    NULL
  }
  lookup(getOption("aisdk.model_pricing")) %||% lookup(aisdk_builtin_model_pricing())
}

#' Register or override pricing for a model
#'
#' Sets a per-model pricing entry in `options(aisdk.model_pricing)` used by
#' [estimate_cost()]. Rates are USD per million tokens.
#'
#' @param model_id Model id (matched exactly or as a prefix, case-insensitive).
#' @param input,output Input/output rate per million tokens.
#' @param cache_read,cache_write Optional cache read/write rates; default to
#'   `input * 0.1` and `input * 1.25` when a run reports cache tokens.
#' @return Invisibly, the updated pricing table.
#' @export
set_model_pricing <- function(model_id, input, output, cache_read = NULL, cache_write = NULL) {
  table <- getOption("aisdk.model_pricing", list())
  entry <- list(input = input, output = output)
  if (!is.null(cache_read)) entry$cache_read <- cache_read
  if (!is.null(cache_write)) entry$cache_write <- cache_write
  table[[model_id]] <- entry
  options(aisdk.model_pricing = table)
  invisible(table)
}

#' Estimate the USD cost of a generation
#'
#' Computes cost from token usage and a per-model pricing table, accounting for
#' cached tokens: Anthropic reports `cache_read_input_tokens` /
#' `cache_creation_input_tokens` separately from `prompt_tokens`, and this
#' prices each at its own (discounted) rate.
#'
#' @param usage A usage list (as on `GenerateResult$usage`) with
#'   `prompt_tokens`, `completion_tokens`, and optionally
#'   `cache_read_input_tokens` / `cache_creation_input_tokens`.
#' @param model_id The model id used, optionally "provider:model".
#' @return The estimated cost in USD (numeric), or `NA_real_` if the model's
#'   pricing is unknown or usage is missing.
#' @export
estimate_cost <- function(usage, model_id) {
  if (is.null(usage)) {
    return(NA_real_)
  }
  pricing <- resolve_model_pricing(model_id)
  if (is.null(pricing) || is.null(pricing$input) || is.null(pricing$output)) {
    return(NA_real_)
  }
  per_m <- function(tok, rate) (tok %||% 0) / 1e6 * (rate %||% 0)
  input_rate <- pricing$input
  cache_read_rate <- pricing$cache_read %||% (input_rate * 0.1)
  cache_write_rate <- pricing$cache_write %||% (input_rate * 1.25)
  per_m(usage$prompt_tokens, input_rate) +
    per_m(usage$completion_tokens, pricing$output) +
    per_m(usage$cache_read_input_tokens, cache_read_rate) +
    per_m(usage$cache_creation_input_tokens, cache_write_rate)
}
