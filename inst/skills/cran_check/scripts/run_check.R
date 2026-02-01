# Run automated CRAN check
# This script encapsulates the rigid parameters required for a valid CRAN check.

# Expects: args$check_dir (optional)
check_dir <- if (!is.null(args$check_dir)) args$check_dir else "check"

message("Starting automated CRAN check...")
message("Check directory: ", check_dir)

# Run the check with standard CRAN flags
results <- devtools::check(
  args = c("--as-cran"),
  error_on = "warning",
  check_dir = check_dir,
  quiet = FALSE
)

# Output summary for the agent to parse
print(results)
