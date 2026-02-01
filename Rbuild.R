#!/usr/bin/env Rscript

# Rbuild.R - Build script for aisdk

message("=> Starting build process for aisdk...")

# Ensure devtools is installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required but not installed.")
}

# 1. Document the package
# Generate: Rd files, Collate field, NAMESPACE. Skip Vignettes.
message("=> Running devtools::document()...")
tryCatch({
  devtools::document(roclets = c("rd", "collate", "namespace"))
  message("=> Documentation completed successfully.")
}, error = function(e) {
  stop("Error during documentation: ", e$message)
})

# 2. Install the package
message("=> Running devtools::install()...")
tryCatch({
  devtools::install(upgrade = "never", quick = TRUE)
  message("=> Installation completed successfully.")
}, error = function(e) {
  stop("Error during installation: ", e$message)
})

# 3. Check the package
message("=> Running devtools::check()...")
tryCatch({
  # Use document=FALSE as we handled it in step 1
  # Run CRAN check with specified arguments to skip manual and vignettes
  devtools::check(
    document = FALSE,
    cran = TRUE,
    args = c("--no-manual", "--no-vignettes", "--no-build-vignettes")
  )
  message("=> Check completed successfully.")
}, error = function(e) {
  message("WARNING: Check returned errors or warnings: ", e$message)
  # We generally don't stop the build script for check warnings, but you could:
  # stop("Check failed: ", e$message)
})

message("=> Build process finished.")
