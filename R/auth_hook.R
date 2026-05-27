#' @title Human-in-the-Loop Authorization
#' @description
#' Provides dynamic authorization hooks to pause Agent execution and request
#' user permission for elevated risk operations.
#'
#' @name auth_hook
NULL

#' Request User Authorization (HITL)
#'
#' Pauses execution and prompts the user for permission to execute a potentially
#' risky action. Supports console environments via `readline`.
#'
#' @param action Character string describing the action the Agent wants to perform.
#' @param risk_level Character string. One of "GREEN", "YELLOW", "RED".
#' @return Logical TRUE if user permits, otherwise throws an error with the rejection reason.
#' @export
request_authorization <- function(action, risk_level = "YELLOW") {
    if (risk_level == "GREEN") {
        return(TRUE)
    }
    if (risk_level == "RED") {
        rlang::abort(sprintf("Action Rejected (RED Risk): %s is strictly prohibited.", action))
    }

    # Yellow risk - request permission
    msg <- sprintf("\n\n[Agent Authorization Request - %s Risk]\nAction: %s\nAllow? (Y/N/Modify): ", risk_level, action)

    # Check if we are in an interactive session
    if (interactive()) {
        response <- readline(prompt = msg)
        response <- trimws(response)

        if (toupper(response) == "Y" || toupper(response) == "YES") {
            return(TRUE)
        } else if (toupper(response) == "N" || toupper(response) == "NO") {
            rlang::abort("User Rejected Action: The user explicitly denied permission for this operation.")
        } else {
            rlang::abort(sprintf("User Rejected Action with Feedback: %s", response))
        }
    } else {
        # In non-interactive mode without a custom hook, default to rejecting yellow operations
        # to ensure safety. In a Shiny/UI environment, developers should mock/override this.
        rlang::abort("Action Rejected: Interactive authorization required for YELLOW risk operations, but session is not interactive.")
    }
}
