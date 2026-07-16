#' @name subscription_proxy
#' @title Use a Codex or Claude subscription via a local proxy
#' @description
#' Convenience helpers that connect aisdk to a **locally running OAuth proxy**
#' which exposes a standard OpenAI- or Anthropic-compatible endpoint backed by
#' your Codex (ChatGPT) or Claude Code (Pro/Max) subscription.
#'
#' aisdk itself never reads, stores, or refreshes any subscription OAuth token
#' and never impersonates an official client. It only talks to a normal
#' compatible endpoint you point it at, exactly like any other custom provider
#' (see [create_custom_provider()]). Running such a proxy is a personal-use,
#' terms-of-service-sensitive choice that lives entirely on your side.
#'
#' @section Proxies:
#' These helpers default to the most common single proxy per vendor, but any
#' compatible proxy works via the `base_url`/`port` arguments:
#' \itemize{
#'   \item Codex: `openai-oauth` (`npx openai-oauth`) at
#'     `http://127.0.0.1:10531/v1` (OpenAI Chat Completions compatible).
#'   \item Claude: `claude-auth-proxy` at `http://127.0.0.1:3000` (Anthropic
#'     Messages compatible; requests hit `/v1/messages`).
#' }
#'
#' @section Terms of service:
#' Subscription OAuth is licensed for individual, interactive use. Do not pool
#' tokens, serve other users, or run a hosted/multi-user backend on it. As of
#' February 2026, Anthropic explicitly prohibits using Claude Free/Pro/Max OAuth
#' tokens in any tool other than Claude Code and Claude.ai (including via the
#' Agent SDK), and enforces this technically. Use these helpers only for your
#' own personal scripts, and switch to a Console API key for anything else.
NULL

# Built-in defaults per vendor. Kept intentionally small (one primary proxy
# each); everything is overridable via arguments or env vars. To bless more
# proxies later (codex-as-api, horselock/claude-code-proxy, ...), extend this
# table and detect_subscription_proxies() below.
.subscription_proxy_defaults <- list(
  codex = list(
    port = 10531L,
    path = "/v1",
    api_format = "chat_completions",
    env = "AISDK_CODEX_PROXY_URL",
    health = "/v1/models",
    ready = "/v1/models",
    start_hint = "Start it with:  npx openai-oauth"
  ),
  claude = list(
    port = 3000L,
    path = "/v1",
    api_format = "anthropic_messages",
    env = "AISDK_CLAUDE_PROXY_URL",
    health = "/health",
    ready = "/ready",
    start_hint = "Start it with the claude-auth-proxy binary (`claude-auth-proxy run`)."
  )
)

# Resolve the base_url for a vendor. Priority, most specific first:
#   1. explicit `base_url` argument
#   2. explicit `port` argument (localhost:<port><path>)
#   3. vendor env override (AISDK_CODEX_PROXY_URL / AISDK_CLAUDE_PROXY_URL)
#   4. built-in localhost default
# Pure resolution: never performs network I/O (auto-detection is a separate,
# explicit step via detect_subscription_proxies()).
resolve_proxy_base_url <- function(kind, base_url = NULL, port = NULL) {
  spec <- .subscription_proxy_defaults[[kind]]
  env_url <- Sys.getenv(spec$env, unset = "")
  raw <- if (!is.null(base_url) && nzchar(base_url)) {
    base_url
  } else if (!is.null(port)) {
    sprintf("http://127.0.0.1:%d%s", as.integer(port), spec$path)
  } else if (nzchar(env_url)) {
    env_url
  } else {
    sprintf("http://127.0.0.1:%d%s", spec$port, spec$path)
  }
  urls <- normalize_base_urls(raw)
  if (length(urls) == 0L) {
    rlang::abort(sprintf("Could not resolve a proxy base URL for `%s`.", kind))
  }
  urls[[1]]
}

# Strip a trailing /v1 (with optional slash) to get the proxy root, used to
# build health/ready endpoints that live outside the API path.
proxy_root_url <- function(base_url) {
  sub("/v1/?$", "", base_url)
}

# Low-level probe of a single URL. Returns list(reachable, status, body).
# Extracted so tests can mock it via local_mocked_bindings().
probe_proxy_endpoint <- function(url, timeout = 2) {
  out <- list(reachable = FALSE, status = NA_integer_, body = NULL)
  tryCatch(
    {
      req <- httr2::request(url)
      req <- httr2::req_timeout(req, timeout)
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      out$status <- httr2::resp_status(resp)
      out$reachable <- TRUE
      out$body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
    },
    error = function(e) NULL
  )
  out
}

#' Detect locally running subscription proxies
#'
#' Probes the built-in localhost ports for known Codex/Claude OAuth proxies and
#' reports which ones appear to be running.
#'
#' @param timeout Per-probe timeout in seconds.
#' @return A data frame with columns `kind`, `base_url`, `api_format`, and
#'   `running`.
#' @seealso [use_codex_subscription()], [use_claude_subscription()],
#'   [subscription_proxy_status()]
#' @export
detect_subscription_proxies <- function(timeout = 2) {
  kinds <- names(.subscription_proxy_defaults)
  rows <- lapply(kinds, function(kind) {
    spec <- .subscription_proxy_defaults[[kind]]
    base_url <- resolve_proxy_base_url(kind)
    root <- proxy_root_url(base_url)
    probe <- probe_proxy_endpoint(paste0(root, spec$health), timeout = timeout)
    data.frame(
      kind = kind,
      base_url = base_url,
      api_format = spec$api_format,
      running = isTRUE(probe$reachable) && !is.na(probe$status) && probe$status < 500,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Check the status of a subscription proxy
#'
#' Probes a Codex or Claude local proxy for reachability and, when the proxy
#' exposes it, whether subscription credentials are loaded.
#'
#' @param kind Which proxy to check: `"all"` (default), `"codex"`, or
#'   `"claude"`.
#' @param base_url Optional explicit base URL to check instead of the resolved
#'   default. Only meaningful together with a single `kind`.
#' @param timeout Per-probe timeout in seconds.
#' @return A data frame with columns `kind`, `url`, `reachable`, `auth_ready`,
#'   and `hint`.
#' @seealso [use_codex_subscription()], [use_claude_subscription()]
#' @export
subscription_proxy_status <- function(kind = c("all", "codex", "claude"),
                                      base_url = NULL,
                                      timeout = 2) {
  kind <- match.arg(kind)
  kinds <- if (kind == "all") names(.subscription_proxy_defaults) else kind
  if (!is.null(base_url) && length(kinds) != 1L) {
    rlang::abort("`base_url` can only be supplied with a single `kind`.")
  }

  rows <- lapply(kinds, function(k) {
    spec <- .subscription_proxy_defaults[[k]]
    resolved <- resolve_proxy_base_url(k, base_url = base_url)
    root <- proxy_root_url(resolved)

    health_url <- paste0(root, spec$health)
    ready_url <- paste0(root, spec$ready)
    health <- probe_proxy_endpoint(health_url, timeout = timeout)
    reachable <- isTRUE(health$reachable) && !is.na(health$status) && health$status < 500

    if (!reachable) {
      auth_ready <- FALSE
      hint <- sprintf("No proxy reachable at %s. %s", resolved, spec$start_hint)
    } else {
      # Reuse the health probe when the readiness endpoint is the same URL
      # (e.g. Codex proxies expose only /v1/models) to avoid a redundant call.
      ready <- if (identical(ready_url, health_url)) {
        health
      } else {
        probe_proxy_endpoint(ready_url, timeout = timeout)
      }
      auth_ready <- isTRUE(ready$reachable) && !is.na(ready$status) && ready$status < 400
      hint <- if (auth_ready) {
        "Proxy is up and subscription credentials are loaded."
      } else {
        "Proxy is up but not ready. Log in to the proxy so it can load your subscription credentials."
      }
    }

    data.frame(
      kind = k,
      url = resolved,
      reachable = reachable,
      auth_ready = auth_ready,
      hint = hint,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Shared implementation for the two vendor helpers.
use_subscription_proxy <- function(kind,
                                    name,
                                    base_url,
                                    port,
                                    model,
                                    api_format,
                                    api_key,
                                    supports_native_tools,
                                    set_as_default,
                                    check,
                                    quiet,
                                    tos_notice) {
  if (!is.character(name) || length(name) != 1L || !nzchar(trimws(name))) {
    rlang::abort("`name` must be a non-empty string.")
  }
  spec <- .subscription_proxy_defaults[[kind]]
  api_format <- api_format %||% spec$api_format
  api_key <- api_key %||% "unused" # non-empty placeholder; the proxy injects real auth

  # Validate set_as_default before any registration side effect.
  if (isTRUE(set_as_default) && (is.null(model) || !nzchar(model))) {
    rlang::abort("`set_as_default = TRUE` requires a non-empty `model`.")
  }

  resolved <- resolve_proxy_base_url(kind, base_url = base_url, port = port)

  if (!isTRUE(quiet)) {
    rlang::inform(
      tos_notice,
      .frequency = "once",
      .frequency_id = paste0("aisdk_subscription_proxy_", kind)
    )
  }

  # Register a lazy factory so the provider is (re)built with fresh config on
  # demand and resolves as "<name>:<model_id>". Reuses the existing custom
  # provider pipeline end to end.
  register_provider(name, local({
    captured <- list(
      name = name,
      base_url = resolved,
      api_format = api_format,
      api_key = api_key,
      supports_native_tools = isTRUE(supports_native_tools)
    )
    function() {
      create_custom_provider(
        provider_name = captured$name,
        base_url = captured$base_url,
        api_key = captured$api_key,
        api_format = captured$api_format,
        supports_native_tools = captured$supports_native_tools,
        responses_state_mode = "stateless"
      )
    }
  }))

  if (isTRUE(set_as_default)) {
    set_model(paste0(name, ":", model))
  }

  # Optional reachability probe. Never blocks registration; only advises.
  if (isTRUE(check)) {
    status <- tryCatch(
      subscription_proxy_status(kind = kind, base_url = resolved),
      error = function(e) NULL
    )
    if (!is.null(status) && !isTRUE(status$reachable[[1]])) {
      rlang::warn(status$hint[[1]])
    }
  }

  if (!isTRUE(quiet)) {
    default_note <- if (isTRUE(set_as_default)) {
      sprintf(" (default model set to \"%s:%s\")", name, model)
    } else {
      sprintf(" (use model \"%s:<model>\")", name)
    }
    message(sprintf("Registered provider '%s' -> %s%s.", name, resolved, default_note))
  }

  invisible(list(
    provider = name,
    base_url = resolved,
    api_format = api_format,
    model = model
  ))
}

#' Use your Codex (ChatGPT) subscription via a local proxy
#'
#' Registers an aisdk provider that routes requests to a locally running
#' Codex OAuth proxy (default: `openai-oauth` at `http://127.0.0.1:10531/v1`),
#' so calls bill against your ChatGPT subscription instead of API credits.
#'
#' @param name Provider id to register (referenced as `"<name>:<model>"`).
#' @param base_url Explicit proxy base URL. Highest priority. When `NULL`, aisdk
#'   resolves the URL from an explicit `port`, then the `AISDK_CODEX_PROXY_URL`
#'   env var, then the localhost default (`http://127.0.0.1:10531/v1`).
#' @param port Localhost port of the proxy. When `NULL` (default), the env var
#'   or the built-in default port is used. Ignored if `base_url` is given.
#' @param model Optional default model id (e.g. `"gpt-5.4-codex"`). Only used
#'   when `set_as_default = TRUE`.
#' @param api_format Wire format the proxy speaks: `"chat_completions"`
#'   (default) or `"responses"`.
#' @param api_key Placeholder credential sent to the proxy (default `"unused"`).
#'   Most OAuth proxies ignore it and inject the real subscription token; set
#'   this only if your proxy expects a shared secret.
#' @param supports_native_tools Whether the proxy supports native tool calling
#'   (default `TRUE`). Set `FALSE` to fall back to text-embedded tool calls.
#' @param set_as_default If `TRUE`, set `"<name>:<model>"` as the session
#'   default model (requires `model`).
#' @param check If `TRUE` (default), probe the proxy and warn if it is not
#'   reachable.
#' @param quiet Suppress the one-time personal-use notice and the confirmation
#'   message.
#' @return Invisibly, a list with `provider`, `base_url`, `api_format`, and
#'   `model`.
#' @seealso [use_claude_subscription()], [subscription_proxy_status()],
#'   [create_custom_provider()]
#' @examples
#' \dontrun{
#' use_codex_subscription()
#' subscription_proxy_status("codex")
#' generate_text(model = "codex:gpt-5.4-codex", prompt = "Hello")
#' }
#' @export
use_codex_subscription <- function(name = "codex",
                                   base_url = NULL,
                                   port = NULL,
                                   model = NULL,
                                   api_format = c("chat_completions", "responses"),
                                   api_key = "unused",
                                   supports_native_tools = TRUE,
                                   set_as_default = FALSE,
                                   check = TRUE,
                                   quiet = FALSE) {
  api_format <- match.arg(api_format)
  use_subscription_proxy(
    kind = "codex",
    name = name,
    base_url = base_url,
    port = port,
    model = model,
    api_format = api_format,
    api_key = api_key,
    supports_native_tools = supports_native_tools,
    set_as_default = set_as_default,
    check = check,
    quiet = quiet,
    tos_notice = paste(
      "Using your Codex/ChatGPT subscription via a local proxy is for personal,",
      "individual use only. Do not pool tokens or serve other users; use an",
      "OpenAI API key for automation and production."
    )
  )
}

#' Use your Claude Code (Pro/Max) subscription via a local proxy
#'
#' Registers an aisdk provider that routes requests to a locally running
#' Claude OAuth proxy (default: `claude-auth-proxy` at `http://127.0.0.1:3000`,
#' Anthropic Messages compatible), so calls bill against your Claude
#' subscription instead of API credits.
#'
#' @param name Provider id to register (referenced as `"<name>:<model>"`).
#' @param base_url Explicit proxy base URL. Highest priority. When `NULL`, aisdk
#'   resolves the URL from an explicit `port`, then the `AISDK_CLAUDE_PROXY_URL`
#'   env var, then the localhost default (`http://127.0.0.1:3000/v1`).
#' @param port Localhost port of the proxy. When `NULL` (default), the env var
#'   or the built-in default port is used. Ignored if `base_url` is given.
#' @param model Optional default model id (e.g. `"claude-sonnet-4-6"`). Only
#'   used when `set_as_default = TRUE`.
#' @param api_key Placeholder credential sent to the proxy (default `"unused"`).
#'   Most OAuth proxies ignore it and inject the real subscription token; set
#'   this only if your proxy expects a shared secret.
#' @param supports_native_tools Whether the proxy supports native tool calling
#'   (default `TRUE`). Set `FALSE` to fall back to text-embedded tool calls.
#' @param set_as_default If `TRUE`, set `"<name>:<model>"` as the session
#'   default model (requires `model`).
#' @param check If `TRUE` (default), probe the proxy and warn if it is not
#'   reachable.
#' @param quiet Suppress the one-time terms-of-service notice and the
#'   confirmation message.
#' @return Invisibly, a list with `provider`, `base_url`, `api_format`, and
#'   `model`.
#' @seealso [use_codex_subscription()], [subscription_proxy_status()],
#'   [create_custom_provider()]
#' @examples
#' \dontrun{
#' use_claude_subscription()
#' subscription_proxy_status("claude")
#' generate_text(model = "claude:claude-sonnet-4-6", prompt = "Hello")
#' }
#' @export
use_claude_subscription <- function(name = "claude",
                                    base_url = NULL,
                                    port = NULL,
                                    model = NULL,
                                    api_key = "unused",
                                    supports_native_tools = TRUE,
                                    set_as_default = FALSE,
                                    check = TRUE,
                                    quiet = FALSE) {
  use_subscription_proxy(
    kind = "claude",
    name = name,
    base_url = base_url,
    port = port,
    model = model,
    api_format = "anthropic_messages",
    api_key = api_key,
    supports_native_tools = supports_native_tools,
    set_as_default = set_as_default,
    check = check,
    quiet = quiet,
    tos_notice = paste(
      "Anthropic's terms (Feb 2026) restrict Claude Free/Pro/Max OAuth tokens to",
      "Claude Code and Claude.ai only; using them elsewhere is not permitted and",
      "is enforced technically. These helpers only talk to a proxy you run",
      "yourself, for personal use. Use a Console API key for anything else."
    )
  )
}
