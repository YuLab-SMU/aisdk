# Initialize a new skill structure using aisdk
# Expects args$name (skill name) and args$path (parent directory)

if (is.null(args$name)) stop("Skill name is required")
path <- if (!is.null(args$path)) args$path else "inst/skills"

# Ensure package is loaded
library(aisdk)

# Create the skill
message("Initializing skill: ", args$name, " in ", path)
aisdk::init_skill(name = args$name, path = path)

# Return success message
print(paste("Skill initialized at", file.path(path, args$name)))
