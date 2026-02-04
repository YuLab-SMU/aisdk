#' @title Console Agent: Intelligent Terminal Assistant
#' @description
#' Creates a default agent for console_chat() that enables natural language
#' interaction with the terminal. Users can ask the agent to run commands,
#' execute R code, read/write files, and more through conversational language.
#'
#' @name console_agent
NULL

#' @title Create Console Tools
#' @description
#' Create a set of tools optimized for console/terminal interaction.
#' Includes computer tools (bash, read_file, write_file, execute_r_code)
#' plus additional console-specific tools.
#'
#' @param working_dir Working directory (default: current directory).
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none" (default: "permissive").
#' @return A list of Tool objects.
#' @export
#' @examples
#' \dontrun{
#' tools <- create_console_tools()
#' # Use with an agent or session
#' session <- create_chat_session(model = "openai:gpt-4o", tools = tools)
#' }
create_console_tools <- function(working_dir = getwd(), sandbox_mode = "permissive") {
    # Get base computer tools
    computer_tools <- create_computer_tools(
        working_dir = working_dir,
        sandbox_mode = sandbox_mode
    )

    # Additional console-specific tools
    console_specific <- list(
        # List directory with details
        tool(
            name = "list_directory",
            description = paste(
                "List files and directories with details (size, modification time).",
                "Use this to explore directory contents before reading files.",
                "Returns formatted listing similar to 'ls -la'."
            ),
            parameters = z_object(
                path = z_string("Directory path to list (default: current directory)"),
                pattern = z_string("Optional glob pattern to filter results (e.g., '*.R')")
            ),
            execute = function(path = ".", pattern = NULL) {
                full_path <- if (grepl("^/|^[A-Za-z]:", path)) {
                    path
                } else {
                    file.path(working_dir, path)
                }

                if (!dir.exists(full_path)) {
                    return(paste("Directory not found:", path))
                }

                files <- list.files(full_path, pattern = pattern, full.names = TRUE)
                if (length(files) == 0) {
                    return("No files found.")
                }

                # Get file info
                info <- file.info(files)
                info$name <- basename(files)
                info$type <- ifelse(info$isdir, "dir", "file")

                # Format output
                lines <- mapply(function(name, size, mtime, type) {
                    size_str <- if (type == "dir") {
                        "<DIR>"
                    } else {
                        format(size, big.mark = ",")
                    }
                    sprintf(
                        "%-6s %10s  %s  %s",
                        type, size_str, format(mtime, "%Y-%m-%d %H:%M"), name
                    )
                }, info$name, info$size, info$mtime, info$type, SIMPLIFY = TRUE)

                paste(c(
                    paste("Directory:", full_path),
                    paste("Total:", length(files), "items"),
                    "",
                    lines
                ), collapse = "\n")
            },
            layer = "computer"
        ),

        # Find files by pattern
        tool(
            name = "find_files",
            description = paste(
                "Search for files matching a pattern in the directory tree.",
                "Use this to locate files before reading or editing them.",
                "Supports glob patterns like '*.R' or 'test*.csv'."
            ),
            parameters = z_object(
                pattern = z_string("File pattern to search for (e.g., '*.R', 'data*.csv')"),
                path = z_string("Starting directory (default: current directory)"),
                recursive = z_boolean("Search recursively in subdirectories (default: TRUE)")
            ),
            execute = function(pattern, path = ".", recursive = TRUE) {
                full_path <- if (grepl("^/|^[A-Za-z]:", path)) {
                    path
                } else {
                    file.path(working_dir, path)
                }

                if (!dir.exists(full_path)) {
                    return(paste("Directory not found:", path))
                }

                files <- list.files(
                    full_path,
                    pattern = utils::glob2rx(pattern),
                    recursive = recursive,
                    full.names = FALSE
                )

                if (length(files) == 0) {
                    return(paste("No files matching pattern:", pattern))
                }

                paste(c(
                    paste("Found", length(files), "files matching:", pattern),
                    "",
                    files
                ), collapse = "\n")
            },
            layer = "computer"
        ),

        # Get system info
        tool(
            name = "get_system_info",
            description = paste(
                "Get system information including OS, R version, and working directory.",
                "Use this to understand the environment before running commands."
            ),
            parameters = NULL,
            execute = function() {
                info <- Sys.info()
                r_info <- R.Version()

                paste(c(
                    "System Information",
                    "==================",
                    paste("OS:", info["sysname"], info["release"]),
                    paste("Machine:", info["machine"]),
                    paste("User:", info["user"]),
                    paste("R Version:", r_info$version.string),
                    paste("Working Directory:", working_dir),
                    paste("Home Directory:", Sys.getenv("HOME")),
                    paste("Temp Directory:", tempdir())
                ), collapse = "\n")
            },
            layer = "computer"
        ),

        # Get environment variables
        tool(
            name = "get_environment",
            description = paste(
                "Get environment variable values.",
                "Use this to check PATH, API keys (masked), or custom environment settings."
            ),
            parameters = z_object(
                names = z_string("Comma-separated list of environment variable names to retrieve. If empty or omitted, returns common variables like PATH, HOME, USER, etc.")
            ),
            execute = function(names = NULL) {
                # Handle comma-separated string input
                if (!is.null(names) && is.character(names) && length(names) == 1 && grepl(",", names)) {
                    names <- trimws(strsplit(names, ",")[[1]])
                }
                if (is.null(names) || length(names) == 0 || (length(names) == 1 && !nzchar(names))) {
                    # Return common environment variables
                    names <- c("PATH", "HOME", "USER", "SHELL", "LANG", "R_HOME", "R_LIBS_USER")
                }

                values <- sapply(names, function(n) {
                    val <- Sys.getenv(n, unset = NA)
                    if (is.na(val)) {
                        "(not set)"
                    } else if (grepl("KEY|TOKEN|SECRET|PASSWORD", n, ignore.case = TRUE)) {
                        # Mask sensitive values
                        if (nchar(val) > 8) {
                            paste0(substr(val, 1, 4), "...", substr(val, nchar(val) - 3, nchar(val)))
                        } else {
                            "****"
                        }
                    } else {
                        val
                    }
                })

                paste(mapply(function(n, v) {
                    paste0(n, "=", v)
                }, names, values), collapse = "\n")
            },
            layer = "computer"
        )
    )

    # Combine all tools
    c(computer_tools, console_specific)
}


#' @title Create Console Agent
#' @description
#' Create the default intelligent terminal agent for console_chat().
#' This agent can execute commands, manage files, and run R code through
#' natural language interaction.
#'
#' @param working_dir Working directory (default: current directory).
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none" (default: "permissive").
#' @param additional_tools Optional list of additional Tool objects to include.
#' @param language Language for responses: "auto", "en", or "zh" (default: "auto").
#' @return An Agent object configured for console interaction.
#' @export
#' @examples
#' \dontrun{
#' # Create default console agent
#' agent <- create_console_agent()
#'
#' # Create with custom working directory
#' agent <- create_console_agent(working_dir = "~/projects/myapp")
#'
#' # Use with console_chat
#' console_chat("openai:gpt-4o", agent = agent)
#' }
create_console_agent <- function(working_dir = getwd(),
                                 sandbox_mode = "permissive",
                                 additional_tools = NULL,
                                 language = "auto") {
    # Resolve working directory
    working_dir <- normalizePath(working_dir, mustWork = FALSE)

    # Create console tools
    tools <- create_console_tools(
        working_dir = working_dir,
        sandbox_mode = sandbox_mode
    )

    # Add any additional tools
    if (!is.null(additional_tools)) {
        tools <- c(tools, additional_tools)
    }

    # Build system prompt
    system_prompt <- build_console_system_prompt(working_dir, sandbox_mode, language)

    # Create agent
    Agent$new(
        name = "ConsoleAgent",
        description = "Intelligent terminal assistant for natural language command execution",
        system_prompt = system_prompt,
        tools = tools
    )
}


#' @title Build Console System Prompt
#' @description Build the system prompt for the console agent.
#' @param working_dir Current working directory.
#' @param sandbox_mode Sandbox mode setting.
#' @param language Language preference.
#' @return System prompt string.
#' @keywords internal
build_console_system_prompt <- function(working_dir, sandbox_mode, language) {
    # Detect language preference
    lang_hint <- if (language == "auto") {
        "Respond in the same language as the user's message."
    } else if (language == "zh") {
        "Respond in Chinese (\u4e2d\u6587)."
    } else {
        "Respond in English."
    }

    paste0(
        "You are R AI SDK Terminal Assistant (\u7ec8\u7aef\u52a9\u624b), an intelligent terminal interface.

## Capabilities

You have access to powerful tools to help users interact with their computer:

- **bash**: Execute any shell/terminal command
- **read_file**: Read file contents
- **write_file**: Create or modify files
- **execute_r_code**: Run R code for data analysis and computation
- **list_directory**: List files and directories with details
- **find_files**: Search for files by pattern
- **get_system_info**: Get system and R environment information
- **get_environment**: Check environment variables

## Guidelines

1. **Understand intent**: Parse the user's natural language request carefully
2. **Plan before acting**: For complex multi-step tasks, briefly explain your approach
3. **Be informative**: Show relevant output and explain results clearly
4. **Be safe**: Avoid destructive operations without explicit user confirmation
5. **Be efficient**: Use the most appropriate tool for each task
6. **Be helpful**: Suggest next steps or related commands when useful

## Language

", lang_hint, "

## Safety

You operate in ", sandbox_mode, " sandbox mode.
", if (sandbox_mode == "strict") {
            "- Operations are restricted to the working directory
- Dangerous commands and code patterns are blocked"
        } else if (sandbox_mode == "permissive") {
            "- Dangerous commands (rm -rf /, fork bombs, etc.) are blocked
- Most normal operations are allowed"
        } else {
            "- No restrictions are enforced (use with caution)"
        }, "

For destructive operations (delete, overwrite), always explain what will happen and ask for confirmation.

## Context

- **Working Directory**: ", working_dir, "
- **Operating System**: ", Sys.info()["sysname"], "
- **R Version**: ", R.Version()$version.string, "

You are ready to help. What would you like to do?"
    )
}
