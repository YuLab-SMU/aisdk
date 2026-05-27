#' @title API Diagnostics
#' @description
#' Provides diagnostic tools to test internet connectivity, DNS resolution,
#' and API reachability.
#' @name api_diagnostics
NULL

#' @title Connect and Diagnose API Reachability
#' @description
#' Tests connectivity to a specific LLM, provider, or URL. This is helpful
#' for diagnosing network issues, DNS failures, or SSL problems.
#'
#' @param model Optional. A `LanguageModelV1` object, or a provider name string
#'   (e.g., "openai", "gemini"). If provided, the base URL for that provider will be tested.
#' @param url Optional. A specific URL to test.
#' @param registry Optional ProviderRegistry to use if `model` is a character string.
#' @return A list containing diagnostic results (invisible).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     # Test by passing a URL directly
#'     check_api(url = "https://api.openai.com/v1")
#'
#'     # Test a model directly
#'     model <- create_openai()$language_model("gpt-4o")
#'     check_api(model)
#' }
#' }
check_api <- function(model = NULL, url = NULL, registry = NULL) {
    has_cli <- requireNamespace("cli", quietly = TRUE)

    print_h1 <- function(msg) if (has_cli) cli::cli_h1(msg) else message(paste0("\n=== ", msg, " ==="))
    print_h2 <- function(msg) if (has_cli) cli::cli_h2(msg) else message(paste0("\n--- ", msg, " ---"))
    print_success <- function(msg) if (has_cli) cli::cli_alert_success(msg) else message(paste0("[OK] ", msg))
    print_danger <- function(msg) if (has_cli) cli::cli_alert_danger(msg) else message(paste0("[ERROR] ", msg))
    print_info <- function(msg) if (has_cli) cli::cli_alert_info(msg) else message(paste0("[INFO] ", msg))
    print_warning <- function(msg) if (has_cli) cli::cli_alert_warning(msg) else message(paste0("[WARNING] ", msg))
    print_text <- function(msg) if (has_cli) cli::cli_text(msg) else message(msg)
    print_bullets <- function(msg) if (has_cli) cli::cli_bullets(msg) else message(paste0("  * ", msg))
    print_rule <- function() if (has_cli) cli::cli_rule() else message("----------------------------------------")

    target_url <- NULL

    if (!is.null(url)) {
        target_url <- url
    } else if (!is.null(model)) {
        if (is.character(model)) {
            tryCatch(
                {
                    model <- resolve_model(model, registry, type = "language")
                },
                error = function(e) {
                    rlang::abort(c("Could not resolve model ID.", "x" = conditionMessage(e)))
                }
            )
        }

        if (inherits(model, "LanguageModelV1")) {
            if (is.function(model$get_config)) {
                config <- model$get_config()
                if (!is.null(config$base_url)) {
                    target_url <- config$base_url
                } else if (!is.null(config$url)) {
                    target_url <- config$url
                } else if (!is.null(config$endpoint)) {
                    target_url <- config$endpoint
                }
            }

            if (is.null(target_url) && is.function(model$build_payload_internal)) {
                try(
                    {
                        params <- list(messages = list(list(role = "user", content = "test")))
                        payload <- model$build_payload_internal(params)
                        target_url <- payload$url
                    },
                    silent = TRUE
                )
            }
        }
    }

    if (is.null(target_url)) {
        rlang::abort("Could not determine URL to test. Please provide a `url` parameter or a valid model.")
    }

    print_h1("API Diagnostics")
    print_text(paste0("Target URL: ", target_url))

    results <- list()

    # 1. System Internet Connectivity
    print_h2("1. System Internet Connectivity")
    has_net <- curl::has_internet()
    results$has_internet <- has_net
    if (has_net) {
        print_success("Internet connection is available.")
    } else {
        print_danger("No internet connection detected (curl::has_internet() failed).")
    }

    # 2. DNS Resolution
    print_h2("2. DNS Resolution")
    parsed <- httr2::url_parse(target_url)
    host <- parsed$hostname
    results$host <- host

    if (!is.null(host) && nzchar(host)) {
        print_text(paste0("Resolving host: ", host))
        ip <- tryCatch(
            curl::nslookup(host, error = TRUE),
            error = function(e) {
                print_danger(paste0("DNS resolution failed for ", host))
                print_bullets(c("x" = conditionMessage(e)))
                NULL
            }
        )
        if (!is.null(ip)) {
            results$dns_ip <- ip
            print_success(paste0("DNS resolved successfully (IP: ", ip[1], ")."))
        }
    } else {
        print_warning("Could not extract hostname from URL.")
    }

    # 3. HTTP Reachability
    print_h2("3. HTTP Reachability")
    req <- httr2::request(target_url)
    req <- httr2::req_timeout(req, 5)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    print_text(paste0("Sending test request to ", target_url, " (timeout: 5s)..."))
    start_time <- Sys.time()
    resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) {
            print_danger("Connection failed.")
            print_bullets(c("x" = conditionMessage(e)))
            NULL
        }
    )
    end_time <- Sys.time()
    latency <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 2)
    results$latency_ms <- latency

    if (!is.null(resp)) {
        status <- httr2::resp_status(resp)
        results$status_code <- status
        if (status >= 200 && status < 500) {
            print_success(paste0("Successfully reached the API endpoint in ", latency, "ms (Status: ", status, ")."))
            if (status == 401 || status == 403) {
                print_info("Received 401/403. This is expected if API key is not provided in diagnostic check.")
            } else if (status == 404) {
                print_info("Received 404. This is expected if the exact endpoint requires specific methods or payloads.")
            }
        } else {
            print_warning(paste0("Reached the API but received server error status: ", status))
        }
    }

    print_rule()
    print_text("Diagnostics complete.")

    invisible(results)
}
