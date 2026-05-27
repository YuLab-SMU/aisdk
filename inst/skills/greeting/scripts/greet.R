# Greeting Script
#
# Generates a friendly greeting message.
#
# Inputs:
#   args$name (optional): The name of the person to greet. Defaults to "World".
#
# Returns:
#   A greeting string.

# Safe argument handling
name <- if (!is.null(args[["name"]]) && nzchar(args[["name"]])) args[["name"]] else "World"

# Generate greeting
message <- sprintf("Hello, %s!", name)

# Return result explicitly
return(message)
