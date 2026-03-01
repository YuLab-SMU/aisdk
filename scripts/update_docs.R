#' Update Quarto Documentation Reference API
#'
#' Because Quarto does not natively parse R functions and Roxygen2 comments
#' like `pkgdown`, you need a bridge to generate the `reference/` block.
#'
#' The modern standard for this is the `altdoc` package.
#'
#' To use this script locally, ensure you have it installed:
#' install.packages("altdoc")
#'
#' This script is automatically called by the GitHub Actions workflow.

if (!requireNamespace("altdoc", quietly = TRUE)) {
    message("The 'altdoc' package is not installed. Skipping automatic reference generation.")
    message("To generate function references for Quarto, please run: install.packages('altdoc')")
} else {
    message("Generating reference documents via altdoc...")

    # altdoc::render_docs() generates the api reference into docs/ or your specific output directory
    # You might need to adjust settings if you want it specifically in reference/
    # For a full setup, you can initialize altdoc via: altdoc::setup_docs(tool = "quarto_website")

    tryCatch(
        {
            altdoc::render_docs()
            message("Reference documentation updated successfully.")
        },
        error = function(e) {
            message("Failed to render docs via altdoc: ", conditionMessage(e))
        }
    )
}
