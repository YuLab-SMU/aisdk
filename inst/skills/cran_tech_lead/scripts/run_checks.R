# Run automated CRAN check via devtools
# This script is used by the Tech Lead agent to verify package technical compliance.

# Expects: args$check_dir (optional)
check_dir <- if (!is.null(args$check_dir)) args$check_dir else "check_results"

message("Tech Lead: Starting automated CRAN check...")

# Run the check
results <- devtools::check(
  args = c("--as-cran"),
  error_on = "warning",
  check_dir = check_dir,
  quiet = FALSE
)

# Output detailed summary
print(results)
