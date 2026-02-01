# Read and parse package metadata
# This helper reads the DESCRIPTION file and returns it as a structured list.

desc_path <- "DESCRIPTION"

if (!file.exists(desc_path)) {
  stop("DESCRIPTION file not found in current directory.")
}

# Read DESCRIPTION using available tools (e.g. read.dcf)
# We use read.dcf which returns a matrix, convert to list
desc_mat <- read.dcf(desc_path)
desc_list <- as.list(desc_mat[1, ])

# Print for the agent to read
print(desc_list)
