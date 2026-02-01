# Script: write_script.R
# Description: Creates an R script file in the skill's scripts directory.
# Arguments:
#   - args$path: Path to the skill directory.
#   - args$script_name: Name of the script file (e.g., "analyze.R").
#   - args$code: The R code content.

path <- args$path
script_name <- args$script_name
code <- args$code

if (is.null(path) || is.null(script_name) || is.null(code)) {
  stop("All arguments (path, script_name, code) are required.")
}

script_path <- file.path(path, "scripts", script_name)
writeLines(code, script_path)

cat(paste0("Wrote script ", script_name, " to ", script_path, "\n"))
