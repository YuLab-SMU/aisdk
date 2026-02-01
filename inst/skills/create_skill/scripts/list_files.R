# Script: list_files.R
# Description: Lists all files in the skill directory to verify creation.
# Arguments:
#   - args$path: Path to the skill directory.

path <- args$path

if (is.null(path)) {
  stop("Argument 'path' is required.")
}

if (!dir.exists(path)) {
  stop(paste0("Directory not found: ", path))
}

files <- list.files(path, recursive = TRUE, full.names = FALSE)
cat(paste0("Files in ", path, ":\n", paste("-", files, collapse = "\n")))
