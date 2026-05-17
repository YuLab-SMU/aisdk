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
#' The default `"minimal"` profile exposes only `bash`, `read_file`,
#' `write_file`, and `edit_file`. Use `profile = "legacy"` for the prior
#' broad tool surface including R, image, Feishu, skill, and inspection tools.
#'
#'
#' @param working_dir Working directory for sandboxed tool execution. Defaults to `tempdir()`.
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none" (default: "permissive").
#' @param startup_dir R session startup directory used for project-aware context. Defaults to `getwd()`.
#' @param profile Console profile. `"minimal"` is the default Pi-like tool set;
#'   `"legacy"` restores the previous all-in-one console tools.
#' @param extensions Extension loading mode. Defaults to `"auto"`.
#' @return A list of Tool objects.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     tools <- create_console_tools()
#'     # Use with an agent or session
#'     session <- create_chat_session(model = "openai:gpt-4o", tools = tools)
#' }
#' }
create_console_tools <- function(working_dir = tempdir(),
                                 sandbox_mode = "permissive",
                                 startup_dir = working_dir,
                                 profile = c("minimal", "legacy"),
                                 extensions = "auto") {
    profile <- match.arg(profile)
    working_dir <- normalizePath(working_dir, winslash = "/", mustWork = FALSE)
    startup_dir <- normalizePath(startup_dir, winslash = "/", mustWork = FALSE)

    # Get base computer tools
    computer <- Computer$new(
        working_dir = working_dir,
        sandbox_mode = sandbox_mode
    )
    computer_tools <- create_computer_tools(
        computer = computer
    )
    if (identical(profile, "minimal")) {
        return(Filter(
            function(tool_obj) tool_obj$name %in% c("bash", "read_file", "write_file", "edit_file"),
            computer_tools
        ))
    }

    computer_tools <- Filter(
        function(tool_obj) !tool_obj$name %in% c("read_file", "execute_r_code"),
        computer_tools
    )
    r_context_tools <- create_r_context_tools()

    resolve_console_existing_path <- function(path) {
        if (is.null(path) || !nzchar(path)) {
            return(path)
        }
        if (grepl("^https?://|^data:", path, ignore.case = TRUE)) {
            return(path)
        }
        if (grepl("^/|^[A-Za-z]:", path)) {
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }

        startup_candidate <- normalizePath(file.path(startup_dir, path), winslash = "/", mustWork = FALSE)
        if (file.exists(startup_candidate)) {
            return(startup_candidate)
        }

        working_candidate <- normalizePath(file.path(working_dir, path), winslash = "/", mustWork = FALSE)
        if (file.exists(working_candidate)) {
            return(working_candidate)
        }

        startup_candidate
    }

    resolve_console_search_root <- function(path = ".") {
        if (is.null(path) || !nzchar(path) || identical(path, ".")) {
            return(startup_dir)
        }
        if (grepl("^/|^[A-Za-z]:", path)) {
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        normalizePath(file.path(startup_dir, path), winslash = "/", mustWork = FALSE)
    }

    resolve_console_path <- function(path) {
        resolve_console_existing_path(path)
    }

    console_r_prelude <- function() {
        paste(
            sprintf(".aisdk_working_dir <- %s", encodeString(working_dir, quote = "\"")),
            sprintf(".aisdk_startup_dir <- %s", encodeString(startup_dir, quote = "\"")),
            "options(",
            "  aisdk.console_working_dir = .aisdk_working_dir,",
            "  aisdk.console_startup_dir = .aisdk_startup_dir",
            ")",
            "aisdk_resolve_startup_path <- function(path = \".\") {",
            "  normalizePath(file.path(.aisdk_startup_dir, path), winslash = \"/\", mustWork = FALSE)",
            "}",
            "aisdk_resolve_working_path <- function(path = \".\") {",
            "  normalizePath(file.path(.aisdk_working_dir, path), winslash = \"/\", mustWork = FALSE)",
            "}",
            sep = "\n"
        )
    }

    console_image_store <- function(envir) {
        if (is.null(envir) || !is.environment(envir)) {
            return(list())
        }
        store <- envir$.console_image_artifacts %||% list()
        if (!is.list(store)) {
            store <- list()
        }
        store
    }

    save_console_image_store <- function(envir, store) {
        if (!is.null(envir) && is.environment(envir)) {
            assign(".console_image_artifacts", store, envir = envir)
        }
        invisible(store)
    }

    remember_console_image_artifacts <- function(envir, artifacts, kind, model_id, prompt = NULL, source_path = NULL) {
        if (is.null(envir) || !is.environment(envir)) {
            return(invisible(NULL))
        }

        store <- console_image_store(envir)
        next_id <- envir$.console_image_artifact_next_id %||% 1L
        entry <- list(
            artifact_id = sprintf("img-%04d", as.integer(next_id)),
            timestamp = as.character(Sys.time()),
            kind = kind,
            model = model_id,
            prompt = prompt %||% "",
            source_path = source_path %||% "",
            artifacts = artifacts %||% list()
        )
        store <- c(list(entry), store)
        if (length(store) > 20) {
            store <- store[seq_len(20)]
        }
        assign(".console_image_artifact_next_id", as.integer(next_id) + 1L, envir = envir)
        save_console_image_store(envir, store)
    }

    recent_console_image_artifacts <- function(envir, limit = 5L) {
        store <- console_image_store(envir)
        if (length(store) == 0) {
            return(character(0))
        }

        store <- store[seq_len(min(length(store), limit))]
        vapply(seq_along(store), function(i) {
            item <- store[[i]]
            first_path <- ""
            if (length(item$artifacts %||% list()) > 0) {
                first_path <- item$artifacts[[1]]$path %||% item$artifacts[[1]]$uri %||% ""
            }
            sprintf(
                "[%s] %s | model=%s | prompt=%s | path=%s",
                item$artifact_id %||% as.character(i),
                item$kind %||% "unknown",
                item$model %||% "",
                compact_text_preview(item$prompt %||% "", width = 48),
                first_path
            )
        }, character(1))
    }

    latest_console_image_path <- function(envir) {
        store <- console_image_store(envir)
        if (length(store) == 0) {
            return(NULL)
        }
        for (item in store) {
            artifacts <- item$artifacts %||% list()
            if (length(artifacts) > 0) {
                path <- artifacts[[1]]$path %||% artifacts[[1]]$uri %||% NULL
                if (!is.null(path) && nzchar(path)) {
                    return(path)
                }
            }
        }
        NULL
    }

    annotate_artifacts <- function(text, paths = character(0)) {
        out <- text
        if (length(paths) > 0) {
            attr(out, "aisdk_artifacts") <- lapply(paths, function(path) {
                list(path = normalizePath(path, winslash = "/", mustWork = FALSE))
            })
        }
        out
    }

    annotate_console_tool_text <- function(text, messages = character(0), warnings = character(0)) {
        text <- text %||% ""
        attr(text, "aisdk_messages") <- unique(messages[nzchar(messages)])
        attr(text, "aisdk_warnings") <- unique(warnings[nzchar(warnings)])
        text
    }

    is_console_binary_file <- function(path) {
        if (grepl("\\.(png|jpg|jpeg|webp|gif|bmp|tif|tiff|pdf|rds|rda|rdata)$", path, ignore.case = TRUE)) {
            return(TRUE)
        }

        bytes <- tryCatch(readBin(path, what = "raw", n = 1024L), error = function(e) raw(0))
        length(bytes) > 0 && any(bytes == as.raw(0))
    }

    console_binary_file_message <- function(path) {
        paste(
            "File appears to be binary or an image and cannot be read as UTF-8 text:",
            path,
            "Use a vision-capable model for image inspection, or provide text/OCR output."
        )
    }

    image_candidate_score <- function(path, query = NULL) {
        score <- 0
        name <- tolower(basename(path %||% ""))
        full <- tolower(path %||% "")
        query <- trimws(tolower(query %||% ""))

        if (grepl("screenshot|screen|shot|capture", name)) score <- score + 2
        if (grepl("hero|poster|mockup|ui|login|chart|plot|figure|image|photo", name)) score <- score + 1

        if (nzchar(query)) {
            tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", query), "\\s+")[[1]])
            tokens <- tokens[nzchar(tokens)]
            for (token in tokens) {
                if (grepl(token, name, fixed = TRUE)) {
                    score <- score + 3
                } else if (grepl(token, full, fixed = TRUE)) {
                    score <- score + 1
                }
            }
        }

        info <- file.info(path)
        if (!is.na(info$mtime)) {
            age_hours <- as.numeric(difftime(Sys.time(), info$mtime, units = "hours"))
            if (is.finite(age_hours)) {
                if (age_hours < 1) score <- score + 3
                else if (age_hours < 24) score <- score + 2
                else if (age_hours < 168) score <- score + 1
            }
        }

        score
    }

    find_console_image_candidates <- function(path = ".", query = NULL, recursive = TRUE, limit = 10L) {
        full_path <- resolve_console_search_root(path)

        if (!dir.exists(full_path)) {
            return(list(error = paste("Directory not found:", path)))
        }

        files <- list.files(
            full_path,
            recursive = recursive,
            full.names = TRUE,
            ignore.case = TRUE
        )
        image_files <- files[grepl("\\.(png|jpg|jpeg|webp|gif|bmp|tif|tiff)$", files, ignore.case = TRUE)]

        if (length(image_files) == 0) {
            return(list(error = "No image files found."))
        }

        info <- file.info(image_files)
        candidates <- lapply(seq_along(image_files), function(i) {
            list(
                path = image_files[[i]],
                name = basename(image_files[[i]]),
                size = info$size[[i]],
                modified = as.character(info$mtime[[i]]),
                score = image_candidate_score(image_files[[i]], query = query)
            )
        })
        candidates <- candidates[order(vapply(candidates, function(x) -x$score, numeric(1)))]
        candidates[seq_len(min(length(candidates), limit))]
    }

    select_console_image_candidates <- function(envir,
                                                query = NULL,
                                                search_path = ".",
                                                recursive = TRUE,
                                                limit = 10L) {
        recent_path <- latest_console_image_path(envir)
        if (!is.null(recent_path) && nzchar(recent_path)) {
            return(list(
                paths = list(recent_path),
                strategy = "recent"
            ))
        }

        candidates <- find_console_image_candidates(
            path = search_path,
            query = query,
            recursive = recursive,
            limit = limit
        )

        if (!is.null(candidates$error)) {
            return(list(error = candidates$error))
        }

        if (length(candidates) == 1) {
            return(list(paths = list(candidates[[1]]$path), strategy = "single_match", candidates = candidates))
        }

        top_score <- candidates[[1]]$score %||% 0
        second_score <- candidates[[2]]$score %||% -Inf
        if (top_score >= second_score + 2) {
            return(list(paths = list(candidates[[1]]$path), strategy = "best_match", candidates = candidates))
        }

        lines <- vapply(seq_along(candidates), function(i) {
            item <- candidates[[i]]
            sprintf("[%d] score=%d | %s | path=%s", i, item$score %||% 0, item$name %||% "", item$path %||% "")
        }, character(1))

        list(
            ambiguous = TRUE,
            candidates = candidates,
            message = paste(c(
                "Multiple likely image candidates were found.",
                "Use ask_user to let the user choose one, or call find_image_files for a fuller list.",
                "",
                lines
            ), collapse = "\n")
        )
    }

    resolve_console_image_inputs <- function(path = NULL,
                                             paths = NULL,
                                             envir = NULL,
                                             query = NULL,
                                             search_path = ".",
                                             recursive = TRUE) {
        if (!is.null(paths) && length(paths) > 0) {
            resolved <- vapply(paths, resolve_console_path, character(1))
            return(list(paths = as.list(resolved), strategy = "explicit_paths"))
        }

        if (!is.null(path) && nzchar(path %||% "")) {
            return(list(paths = list(resolve_console_path(path)), strategy = "explicit_path"))
        }

        auto <- select_console_image_candidates(
            envir = envir,
            query = query,
            search_path = search_path,
            recursive = recursive
        )
        if (!is.null(auto$paths)) {
            if (identical(auto$strategy %||% "", "recent")) {
                return(list(
                    paths = auto$paths,
                    strategy = auto$strategy,
                    candidates = auto$candidates %||% NULL
                ))
            }
            return(list(
                paths = lapply(auto$paths, resolve_console_path),
                strategy = auto$strategy,
                candidates = auto$candidates %||% NULL
            ))
        }
        auto
    }

    default_console_image_model <- function(provider, purpose = c("generate", "edit")) {
        purpose <- match.arg(purpose)
        switch(provider %||% "",
            openai = "openai:gpt-image-2",
            gemini = "gemini:gemini-2.5-flash-image",
            volcengine = "volcengine:doubao-seedream-5-0",
            xai = "xai:grok-2-image",
            stepfun = if (purpose == "edit") "stepfun:step-1x-edit" else "stepfun:step-1x-medium",
            openrouter = "openrouter:openai/gpt-image-2",
            aihubmix = "aihubmix:gpt-image-2",
            NULL
        )
    }

    resolve_console_vision_model <- function(envir, explicit_model = NULL) {
        explicit_model <- explicit_model %||% ""
        if (nzchar(explicit_model)) {
            return(explicit_model)
        }

        envir_routes <- normalize_capability_model_routes(envir$.capability_models %||% list())
        routed <- envir_routes[["vision.inspect"]]$model %||%
            get_capability_model("vision.inspect", default = NULL)
        if (!is.null(routed)) {
            return(routed)
        }

        current <- envir$.session_model_id %||% ""
        if (nzchar(current)) {
            return(current)
        }

        opt <- getOption("aisdk.console_vision_model", Sys.getenv("AISDK_DEFAULT_VISION_MODEL", ""))
        if (nzchar(opt)) {
            return(opt)
        }

        "openai:gpt-4o"
    }

    console_vision_unavailable_message <- function(model) {
        model_id <- capability_model_label(model)
        paste(
            "The selected language model",
            paste0("`", model_id, "`"),
            "does not advertise multimodal image input support.",
            "I cannot inspect image pixels with this model.",
            "Switch to a vision-capable language model or provide a text description/OCR output."
        )
    }

    resolve_console_image_model <- function(envir, explicit_model = NULL, purpose = c("generate", "edit")) {
        purpose <- match.arg(purpose)
        explicit_model <- explicit_model %||% ""
        if (nzchar(explicit_model)) {
            return(explicit_model)
        }

        capability <- if (purpose == "edit") "image.edit" else "image.generate"
        envir_routes <- normalize_capability_model_routes(envir$.capability_models %||% list())
        routed <- envir_routes[[capability]]$model %||%
            get_capability_model(capability, default = NULL)
        if (!is.null(routed)) {
            return(routed)
        }

        opt_name <- if (purpose == "edit") "aisdk.console_image_edit_model" else "aisdk.console_image_model"
        env_name <- if (purpose == "edit") "AISDK_DEFAULT_IMAGE_EDIT_MODEL" else "AISDK_DEFAULT_IMAGE_MODEL"
        override <- getOption(opt_name, Sys.getenv(env_name, ""))
        if (nzchar(override)) {
            return(override)
        }

        current <- envir$.session_model_id %||% ""
        provider <- if (nzchar(current) && grepl(":", current, fixed = TRUE)) {
            strsplit(current, ":", fixed = TRUE)[[1]][1]
        } else {
            ""
        }

        default_console_image_model(provider, purpose = purpose) %||% "gemini:gemini-2.5-flash-image"
    }

    # Additional console-specific tools
    console_specific <- list(
        tool(
            name = "read_file",
            description = paste(
                "Read the contents of a text file with automatic encoding fallback.",
                "Relative paths prefer the R startup directory so requests about the user's current project work naturally.",
                "Absolute paths are also allowed.",
                "Returns UTF-8 text; if output looks garbled, retry with explicit encoding such as GB18030, GBK, latin1, or CP1252."
            ),
            parameters = z_object(
                path = z_string("Path to the file to read"),
                encoding = z_string("Optional source file encoding to try first, for example UTF-8, GB18030, GBK, BIG5, latin1, or CP1252.", nullable = TRUE),
                .required = "path"
            ),
            execute = function(path, encoding = NULL) {
                result <- tryCatch(
                    {
                        full_path <- resolve_console_existing_path(path)
                        if (!file.exists(full_path)) {
                            list(error = TRUE, message = paste("File not found:", path))
                        } else if (is_console_binary_file(full_path)) {
                            list(error = TRUE, message = console_binary_file_message(path))
                        } else {
                            computer$read_file(full_path, encoding = encoding %||% "UTF-8")
                        }
                    },
                    error = function(e) list(error = TRUE, message = conditionMessage(e))
                )

                if (isTRUE(result$error)) {
                    result$message
                } else {
                    result$content
                }
            },
            layer = "computer"
        ),

        tool(
            name = "execute_r_code",
            description = paste(
                "Execute R code in an isolated sandboxed process.",
                "Relative paths still resolve from the sandbox working directory.",
                "Use `.aisdk_startup_dir` or `aisdk_resolve_startup_path()` when you need files from the user's startup project directory."
            ),
            parameters = z_object(
                code = z_string("R code to execute")
            ),
            execute = function(code) {
                result <- computer$execute_r_code(paste(console_r_prelude(), code, sep = "\n\n"))
                if (result$error) {
                    paste("Error:", result$message)
                } else {
                    annotate_artifacts(
                        structure(
                            paste("Result:", paste(result$output, collapse = "\n")),
                            aisdk_messages = c(
                                result$messages %||% character(0),
                                paste("Sandbox working directory:", working_dir),
                                paste("Startup directory:", startup_dir)
                            ),
                            aisdk_warnings = result$warnings %||% character(0)
                        ),
                        result$created_files %||% character(0)
                    )
                }
            },
            layer = "computer"
        ),

        # Interactive prompt: ask user questions mid-conversation
        tool(
            name = "ask_user",
            description = paste(
                "Ask the user a question interactively in the terminal.",
                "Use this when you need user input, confirmation, or a choice between options.",
                "For multiple-choice: provide 'choices' as an array of option strings.",
                "For yes/no confirmation: omit 'choices' and set 'confirm' to true.",
                "For free-text input: omit 'choices' and set 'confirm' to false (default).",
                "Examples: choosing a package, confirming destructive operations, getting preferences."
            ),
            parameters = z_object(
                question = z_string("The question to present to the user"),
                choices = z_array(
                    z_string("A choice option"),
                    description = "List of options for numbered selection. Omit for yes/no or free-text."
                ),
                confirm = z_boolean("If true (and no choices), present a Yes/No prompt. Default: false.")
            ),
            execute = function(question, choices = NULL, confirm = FALSE) {
                if (!interactive()) {
                    return("Error: Cannot prompt user in non-interactive session.")
                }

                if (!is.null(choices) && length(choices) > 0) {
                    selection <- console_menu(question, choices)
                    if (is.null(selection)) return("User cancelled the selection.")
                    return(paste0("User selected option ", selection, ": ", choices[[selection]]))
                }

                if (isTRUE(confirm)) {
                    result <- console_confirm(question)
                    if (is.null(result)) return("User cancelled.")
                    return(if (result) "User confirmed: Yes" else "User declined: No")
                }

                response <- console_input(question)
                if (is.null(response)) return("User provided no input.")
                paste("User responded:", response)
            },
            layer = "llm"
        ),

        # Execute R code locally in Global Environment
        tool(
            name = "execute_r_code_local",
            description = paste(
                "Execute R code directly in the user's current LIVE R session (Global Environment).",
                "Use this tool ONLY when you need to create, modify, or interact with variables in the user's workspace.",
                "If local mode is not enabled, the user will be prompted to grant permission interactively."
            ),
            parameters = z_object(
                code = z_string("The R code to execute in the user's global environment")
            ),
            execute = function(args) {
                if (!isTRUE(args$.envir$.local_mode)) {
                    if (interactive()) {
                        confirmed <- console_confirm(
                            "This operation requires local execution mode. Enable it now?"
                        )
                        if (isTRUE(confirmed)) {
                            assign(".local_mode", TRUE, envir = args$.envir)
                            cli::cli_alert_success("Local execution mode enabled.")
                        } else {
                            return("User declined to enable local execution mode. Operation cancelled.")
                        }
                    } else {
                        return("Error: Local execution is disabled and cannot prompt in non-interactive session.")
                    }
                }

                captured <- capture_r_execution(
                    eval(parse(text = args$code), envir = globalenv()),
                    envir = globalenv(),
                    auto_print_value = TRUE
                )

                if (!isTRUE(captured$ok)) {
                    return(paste("Error:", captured$error))
                }

                paste(
                    "Execution complete. Output:\n",
                    format_captured_execution(captured)
                )
            },
            layer = "computer"
        ),

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
                full_path <- resolve_console_search_root(path)

                if (!dir.exists(full_path)) {
                    return(paste("Directory not found:", path))
                }

                files <- list.files(full_path, pattern = pattern, full.names = TRUE)
                if (length(files) == 0) {
                    return(paste(c(
                        paste("Directory:", full_path),
                        "Total: 0 items",
                        "",
                        "No files found."
                    ), collapse = "\n"))
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
                full_path <- resolve_console_search_root(path)

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

        tool(
            name = "find_image_files",
            description = paste(
                "Find likely image files in the working directory tree and rank them by relevance.",
                "Use this when the user mentions a screenshot, poster, render, product photo, hero image, chart, or figure but does not give an explicit path.",
                "Prefer this before asking the user for a path when local image files likely exist."
            ),
            parameters = z_object(
                query = z_string("Optional relevance hint such as 'login screenshot' or 'hero image'", nullable = TRUE),
                path = z_string("Starting directory (default: current directory)"),
                recursive = z_boolean("Search recursively in subdirectories (default: TRUE)"),
                limit = z_integer("Maximum number of candidate image files to return (default: 10)", nullable = TRUE)
            ),
            execute = function(query = NULL, path = ".", recursive = TRUE, limit = 10L) {
                candidates <- find_console_image_candidates(
                    path = path,
                    query = query,
                    recursive = recursive,
                    limit = limit %||% 10L
                )

                if (!is.null(candidates$error)) {
                    return(candidates$error)
                }

                lines <- vapply(seq_along(candidates), function(i) {
                    item <- candidates[[i]]
                    sprintf(
                        "[%d] score=%d | %s | modified=%s | path=%s",
                        i,
                        item$score %||% 0,
                        item$name %||% "",
                        item$modified %||% "",
                        item$path %||% ""
                    )
                }, character(1))

                paste(c("Image candidates:", "", lines), collapse = "\n")
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
                    paste("Startup Directory:", startup_dir),
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
        ),

        tool(
            name = "setup_feishu_channel",
            description = paste(
                "Launch an interactive wizard for configuring a Feishu bot connection.",
                "Use this when the user wants to connect Feishu, set up a Feishu bot, configure webhook settings,",
                "or asks to start using aisdk through Feishu without manually editing environment variables."
            ),
            parameters = z_object(
                app_id = z_string(description = "Optional Feishu app id if the user already provided it", nullable = TRUE),
                app_secret = z_string(description = "Optional Feishu app secret if the user already provided it", nullable = TRUE),
                .required = character(0)
            ),
            execute = function(args) {
                if (!interactive()) {
                    return("Error: setup_feishu_channel requires an interactive console session.")
                }

                current_model <- args$.envir$.session_model_id %||% ""

                result <- setup_feishu_channel(
                    prompt_hooks = list(
                        menu = console_menu,
                        input = console_input,
                        confirm = console_confirm,
                        save = update_renviron
                    ),
                    current_model = current_model,
                    app_id = args$app_id %||% NULL,
                    app_secret = args$app_secret %||% NULL,
                    workdir = startup_dir,
                    session_root = file.path(startup_dir, ".aisdk", "feishu")
                )

                result$summary %||% "Feishu setup finished."
            },
            layer = "computer"
        ),

        tool(
            name = "analyze_image_file",
            description = paste(
                "Analyze an image file or image URL with a vision-capable language model.",
                "Use this when the user asks about screenshots, charts, figures, OCR, UI issues, product images, or wants visual understanding.",
                "If model is omitted, reuse the current session model when possible."
            ),
            parameters = z_object(
                path = z_string("Optional local image path or image URL to analyze", nullable = TRUE),
                paths = z_array(z_string("Local image path or image URL to analyze"), description = "Optional batch of image paths or URLs", nullable = TRUE),
                task = z_string("What to analyze or extract from the image"),
                model = z_string("Optional provider:model override for image understanding", nullable = TRUE),
                search_path = z_string("Directory to search when the path is omitted", nullable = TRUE),
                recursive = z_boolean("Search recursively for candidate images when the path is omitted", nullable = TRUE),
                .required = c("task")
            ),
            execute = function(args) {
                model_ref <- resolve_console_vision_model(args$.envir, explicit_model = args$model %||% NULL)
                model_label <- capability_model_label(model_ref)
                if (model_ref_capability_explicitly_unavailable(model_ref, "vision_input")) {
                    return(console_vision_unavailable_message(model_ref))
                }

                resolved <- resolve_console_image_inputs(
                    path = args$path %||% NULL,
                    paths = args$paths %||% NULL,
                    envir = args$.envir,
                    query = args$task,
                    search_path = args$search_path %||% ".",
                    recursive = args$recursive %||% TRUE
                )
                if (!is.null(resolved$error)) {
                    return(resolved$error)
                }
                if (isTRUE(resolved$ambiguous)) {
                    return(resolved$message)
                }

                image_paths <- unlist(resolved$paths %||% list(), use.names = FALSE)
                result <- if (length(image_paths) <= 1) {
                    analyze_image(
                        model = model_ref,
                        image = image_paths[[1]],
                        prompt = args$task
                    )
                } else {
                    generate_text(
                        model = model_ref,
                        prompt = list(list(
                            role = "user",
                            content = c(
                                list(input_text(args$task)),
                                lapply(image_paths, input_image)
                            )
                        ))
                    )
                }

                remember_console_image_artifacts(
                    args$.envir,
                    artifacts = lapply(image_paths, function(p) list(path = p)),
                    kind = "analysis_input",
                    model_id = model_label,
                    prompt = args$task,
                    source_path = paste(image_paths, collapse = ", ")
                )

                annotate_console_tool_text(
                    result$text %||% "Image analysis completed.",
                    messages = c(
                        paste("Vision model:", model_label),
                        paste("Images:", paste(image_paths, collapse = ", ")),
                        paste("Selection strategy:", resolved$strategy %||% "unknown")
                    )
                )
            },
            layer = "computer",
            meta = list(
                required_model_capabilities = c("vision_input"),
                model_capability_route = "vision.inspect"
            )
        ),

        tool(
            name = "extract_from_image_file",
            description = paste(
                "Extract structured or semi-structured information from an image file or image URL.",
                "Use this for OCR, invoice fields, chart labels, table extraction, UI text, or form-like image content.",
                "If schema_json is omitted, return the best structured JSON the model can infer from the task."
            ),
            parameters = z_object(
                path = z_string("Optional local image path or image URL to analyze", nullable = TRUE),
                paths = z_array(z_string("Local image path or image URL to analyze"), description = "Optional batch of image paths or URLs", nullable = TRUE),
                task = z_string("What to extract from the image"),
                schema_json = z_string("Optional JSON schema object as a string", nullable = TRUE),
                model = z_string("Optional provider:model override for image extraction", nullable = TRUE),
                search_path = z_string("Directory to search when the path is omitted", nullable = TRUE),
                recursive = z_boolean("Search recursively for candidate images when the path is omitted", nullable = TRUE),
                .required = c("task")
            ),
            execute = function(args) {
                model_ref <- resolve_console_vision_model(args$.envir, explicit_model = args$model %||% NULL)
                model_label <- capability_model_label(model_ref)
                if (model_ref_capability_explicitly_unavailable(model_ref, "vision_input")) {
                    return(console_vision_unavailable_message(model_ref))
                }

                resolved <- resolve_console_image_inputs(
                    path = args$path %||% NULL,
                    paths = args$paths %||% NULL,
                    envir = args$.envir,
                    query = args$task,
                    search_path = args$search_path %||% ".",
                    recursive = args$recursive %||% TRUE
                )
                if (!is.null(resolved$error)) {
                    return(resolved$error)
                }
                if (isTRUE(resolved$ambiguous)) {
                    return(resolved$message)
                }

                image_paths <- unlist(resolved$paths %||% list(), use.names = FALSE)

                result <- if (!is.null(args$schema_json) && nzchar(args$schema_json) && length(image_paths) == 1) {
                    parsed_schema <- jsonlite::fromJSON(args$schema_json, simplifyVector = FALSE)
                    generate_text(
                        model = model_ref,
                        prompt = list(list(
                            role = "user",
                            content = list(
                                input_text(args$task),
                                input_image(image_paths[[1]])
                            )
                        )),
                        response_format = parsed_schema
                    )
                } else if (!is.null(args$schema_json) && nzchar(args$schema_json) && length(image_paths) > 1) {
                    parsed_schema <- jsonlite::fromJSON(args$schema_json, simplifyVector = FALSE)
                    objects <- lapply(image_paths, function(p) {
                        generate_text(
                            model = model_ref,
                            prompt = list(list(
                                role = "user",
                                content = list(
                                    input_text(args$task),
                                    input_image(p)
                                )
                            )),
                            response_format = parsed_schema
                        )$object
                    })
                    structure(list(object = objects), class = "console_image_batch_result")
                } else {
                    if (length(image_paths) == 1) {
                        analyze_image(
                            model = model_ref,
                            image = image_paths[[1]],
                            prompt = paste(
                                args$task,
                                "\n\nReturn the result in clear JSON with stable keys whenever possible."
                            )
                        )
                    } else {
                        responses <- lapply(image_paths, function(p) {
                            analyze_image(
                                model = model_ref,
                                image = p,
                                prompt = paste(
                                    args$task,
                                    "\n\nReturn the result in clear JSON with stable keys whenever possible."
                                )
                            )$text
                        })
                        structure(list(text = safe_to_json(responses, auto_unbox = TRUE, pretty = TRUE)), class = "console_image_batch_result")
                    }
                }

                remember_console_image_artifacts(
                    args$.envir,
                    artifacts = lapply(image_paths, function(p) list(path = p)),
                    kind = "extraction_input",
                    model_id = model_label,
                    prompt = args$task,
                    source_path = paste(image_paths, collapse = ", ")
                )

                if (!is.null(result$object)) {
                    out <- safe_to_json(result$object, auto_unbox = TRUE, pretty = TRUE)
                } else {
                    out <- result$text %||% "Image extraction completed."
                }

                annotate_console_tool_text(
                    out,
                    messages = c(
                        paste("Vision model:", model_label),
                        paste("Images:", paste(image_paths, collapse = ", ")),
                        paste("Selection strategy:", resolved$strategy %||% "unknown")
                    )
                )
            },
            layer = "computer",
            meta = list(
                required_model_capabilities = c("vision_input"),
                model_capability_route = "vision.inspect"
            )
        ),

        tool(
            name = "generate_image_asset",
            description = paste(
                "Generate an image artifact with an image model.",
                "Use this when the user asks to create artwork, posters, hero images, thumbnails, product shots, or other new images.",
                "The generated image path is remembered for follow-up edits."
            ),
            parameters = z_object(
                prompt = z_string("Image generation prompt"),
                model = z_string("Optional provider:model override for image generation", nullable = TRUE),
                output_dir = z_string("Optional output directory", nullable = TRUE)
            ),
            execute = function(args) {
                model_ref <- resolve_console_image_model(args$.envir, explicit_model = args$model %||% NULL, purpose = "generate")
                model_label <- capability_model_label(model_ref)
                output_dir <- args$output_dir %||% tempdir()
                result <- generate_image(
                    model = model_ref,
                    prompt = args$prompt,
                    output_dir = output_dir
                )

                remember_console_image_artifacts(
                    args$.envir,
                    artifacts = result$images,
                    kind = "generated",
                    model_id = model_label,
                    prompt = args$prompt
                )

                paths <- vapply(result$images %||% list(), function(img) img$path %||% img$uri %||% "", character(1))
                annotate_console_tool_text(paste(c(
                    paste("Generated", length(paths), "image(s)."),
                    if (nzchar(result$text %||% "")) paste("Model note:", result$text) else NULL,
                    paths
                ), collapse = "\n"),
                messages = c(
                    paste("Image model:", model_label),
                    if (length(paths) > 0) paste("Artifacts:", paste(paths, collapse = ", ")) else character(0)
                ))
            },
            layer = "computer"
        ),

        tool(
            name = "edit_image_asset",
            description = paste(
                "Edit an existing image artifact with an image model.",
                "Use this when the user asks to modify a generated image, restyle a product photo, change colors, or transform an existing visual.",
                "If image_path is omitted, reuse the most recent generated or edited image artifact."
            ),
            parameters = z_object(
                image_path = z_string("Optional local path or URL to the source image", nullable = TRUE),
                prompt = z_string("Image editing instruction"),
                model = z_string("Optional provider:model override for image editing", nullable = TRUE),
                mask_path = z_string("Optional local mask path if the provider supports it", nullable = TRUE),
                output_dir = z_string("Optional output directory", nullable = TRUE)
            ),
            execute = function(args) {
                resolved <- resolve_console_image_inputs(
                    path = args$image_path %||% NULL,
                    envir = args$.envir,
                    query = args$prompt,
                    search_path = ".",
                    recursive = TRUE
                )
                if (!is.null(resolved$error)) {
                    return("No prior image artifact is available and no matching image file was found. Provide image_path explicitly first.")
                }
                if (isTRUE(resolved$ambiguous)) {
                    return(resolved$message)
                }

                source_path <- resolved$paths[[1]]
                mask_path <- args$mask_path %||% NULL
                if (!is.null(mask_path) && nzchar(mask_path)) {
                    mask_path <- resolve_console_path(mask_path)
                } else {
                    mask_path <- NULL
                }

                model_ref <- resolve_console_image_model(args$.envir, explicit_model = args$model %||% NULL, purpose = "edit")
                model_label <- capability_model_label(model_ref)
                output_dir <- args$output_dir %||% tempdir()
                result <- edit_image(
                    model = model_ref,
                    image = source_path,
                    prompt = args$prompt,
                    mask = mask_path,
                    output_dir = output_dir
                )

                remember_console_image_artifacts(
                    args$.envir,
                    artifacts = result$images,
                    kind = "edited",
                    model_id = model_label,
                    prompt = args$prompt,
                    source_path = source_path
                )

                paths <- vapply(result$images %||% list(), function(img) img$path %||% img$uri %||% "", character(1))
                annotate_console_tool_text(paste(c(
                    paste("Edited", length(paths), "image(s)."),
                    paste("Source:", source_path),
                    if (nzchar(result$text %||% "")) paste("Model note:", result$text) else NULL,
                    paths
                ), collapse = "\n"),
                messages = c(
                    paste("Image model:", model_label),
                    paste("Source image:", source_path),
                    paste("Selection strategy:", resolved$strategy %||% "unknown"),
                    if (length(paths) > 0) paste("Artifacts:", paste(paths, collapse = ", ")) else character(0)
                ))
            },
            layer = "computer"
        ),

        tool(
            name = "get_recent_image_artifacts",
            description = paste(
                "List recently generated, edited, or analyzed image artifacts remembered by the console agent.",
                "Use this when you want to continue working on a previously created image without asking the user to repeat the path."
            ),
            parameters = z_object(
                limit = z_integer("Maximum number of recent image artifact entries to show", nullable = TRUE)
            ),
            execute = function(args) {
                limit <- args$limit %||% 5L
                lines <- recent_console_image_artifacts(args$.envir, limit = limit)
                if (length(lines) == 0) {
                    return("No recent image artifacts are recorded in this session.")
                }
                paste(c("Recent image artifacts:", "", lines), collapse = "\n")
            },
            layer = "computer"
        )
    )

    # Combine all tools
    c(computer_tools, r_context_tools, console_specific)
}


#' @title Create Console Agent
#' @description
#' Create the default intelligent terminal agent for console_chat().
#' This agent can execute commands, manage files, and run R code through
#' natural language interaction.
#'
#'
#' @param working_dir Working directory for sandboxed tool execution. Defaults to `tempdir()`.
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none" (default: "permissive").
#' @param additional_tools Optional list of additional Tool objects to include.
#' @param language Language for responses: "auto", "en", or "zh" (default: "auto").
#' @param startup_dir R session startup directory used for project-aware context. Defaults to `getwd()`.
#' @param skills Optional skill paths, `"auto"`, or a SkillRegistry object.
#' @param profile Console profile. `"minimal"` is the default Pi-like tool set;
#'   `"legacy"` restores the previous all-in-one console agent.
#' @param extensions Extension loading mode. Defaults to `"auto"`.
#' @return An Agent object configured for console interaction.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     # Create default console agent
#'     agent <- create_console_agent()
#'
#'     # Create with custom working directory
#'     agent <- create_console_agent(working_dir = "~/projects/myapp")
#'
#'     # Use with console_chat
#'     console_chat("openai:gpt-4o", agent = agent)
#' }
#' }
create_console_agent <- function(working_dir = tempdir(),
                                 sandbox_mode = "permissive",
                                 additional_tools = NULL,
                                 language = "auto",
                                 startup_dir = working_dir,
                                 skills = "auto",
                                 profile = c("minimal", "legacy"),
                                 extensions = "auto") {
    profile <- match.arg(profile)
    # Resolve working directory
    working_dir <- normalizePath(working_dir, winslash = "/", mustWork = FALSE)
    startup_dir <- normalizePath(startup_dir, winslash = "/", mustWork = FALSE)

    # Create console tools
    tools <- create_console_tools(
        working_dir = working_dir,
        startup_dir = startup_dir,
        sandbox_mode = sandbox_mode,
        profile = profile,
        extensions = extensions
    )

    # Add any additional tools
    extension_runtime <- console_extension_runtime_load(session = NULL, startup_dir = startup_dir, extensions = extensions)
    extension_tools <- console_extension_tools(extension_runtime)
    if (length(extension_tools) > 0) {
        tools <- c(tools, extension_tools)
    }

    if (!is.null(additional_tools)) {
        tools <- c(tools, additional_tools)
    }

    # Build system prompt
    system_prompt <- build_console_system_prompt(working_dir, startup_dir, sandbox_mode, language, profile = profile)

    skill_registry <- if (identical(profile, "minimal")) {
        NULL
    } else if (identical(skills, "auto")) {
        create_auto_skill_registry(project_dir = startup_dir, recursive = TRUE)
    } else {
        skills
    }

    # Create agent
    Agent$new(
        name = "ConsoleAgent",
        description = "Intelligent terminal assistant for natural language command execution",
        system_prompt = system_prompt,
        tools = tools,
        skills = skill_registry
    )
}


#' @title Build Console System Prompt
#' @description Build the system prompt for the console agent.
#' @param working_dir Sandbox working directory for tool execution.
#' @param startup_dir R session startup directory for project-aware context.
#' @param sandbox_mode Sandbox mode setting.
#' @param language Language preference.
#' @return System prompt string.
#' @keywords internal
build_console_system_prompt <- function(working_dir, startup_dir, sandbox_mode, language, profile = c("minimal", "legacy")) {
    profile <- match.arg(profile)
    # Detect language preference
    lang_hint <- if (language == "auto") {
        "Respond in the same language as the user's message."
    } else if (language == "zh") {
        "Respond in Chinese (\u4e2d\u6587)."
    } else {
        "Respond in English."
    }

    if (identical(profile, "minimal")) {
        return(paste0(
            "You are R AI SDK Terminal Assistant.\n\n",
            "Use a small computer tool set: `bash`, `read_file`, `write_file`, and `edit_file`.\n",
            "Act directly when the task is clear. For file edits, read the file first, then use `edit_file` for targeted replacements or `write_file` for complete writes.\n",
            "If a tool fails, use the error as evidence and change approach instead of repeating the same call.\n",
            "For file encoding errors or garbled text, retry `read_file` with explicit encodings such as `GB18030`, `GBK`, `latin1`, or `CP1252`; use `bash` diagnostics like `file` or `iconv` only when needed.\n",
            "Ask the user only when a decision or missing input blocks progress.\n\n",
            "Language: ", lang_hint, "\n\n",
            "Safety: you operate in ", sandbox_mode, " sandbox mode. Confirm destructive operations before proceeding.\n\n",
            "Context:\n",
            "- Working Directory: ", working_dir, "\n",
            "- R Startup Directory: ", startup_dir, "\n",
            "- Operating System: ", Sys.info()["sysname"], "\n",
            "- R Version: ", R.Version()$version.string, "\n"
        ))
    }

    paste0(
        "You are R AI SDK Terminal Assistant (\u7ec8\u7aef\u52a9\u624b), an intelligent terminal interface.

## Capabilities

You have access to powerful tools to help users interact with their computer:

- **ask_user**: Ask the user questions interactively (choices, confirmations, free-text)
- **bash**: Execute any shell/terminal command
- **read_file**: Read file contents
- **write_file**: Create or modify files
- **execute_r_code**: Run R code for data analysis and computation (Sandboxed)
- **execute_r_code_local**: Run R code directly in the user's workspace (auto-prompts for permission)
- **list_r_objects**: List read-only objects from the session environment and RStudio/.GlobalEnv workspace
- **inspect_r_object**: Inspect read-only structure of session or RStudio/.GlobalEnv objects
- **list_directory**: List files and directories with details
- **find_files**: Search for files by pattern
- **find_image_files**: Search for likely image files by relevance
- **get_system_info**: Get system and R environment information
- **get_environment**: Check environment variables
- **setup_feishu_channel**: Guided Feishu bot setup wizard for non-developer users
- **analyze_image_file**: Analyze screenshots, charts, figures, and other images when the active language model supports vision input
- **extract_from_image_file**: Extract OCR-like or structured data from images when the active language model supports vision input
- **generate_image_asset**: Generate new image assets with an image model
- **edit_image_asset**: Modify an existing image asset with an image model
- **get_recent_image_artifacts**: Recall recently generated or edited image paths

## Interactive Prompts

Use **ask_user** to get real-time feedback from the user during task execution:

- **Multiple choice**: When there are several valid approaches (e.g., which plotting library, which file format)
- **Confirmation**: Before destructive operations (delete, overwrite) or operations with side effects
- **Free-text input**: When you need specific values (file paths, variable names, parameters)

Prefer interactive prompts over generating text that asks the user to reply. This provides a much better UX.

## Guidelines

1. **Understand intent**: Parse the user's natural language request carefully
2. **Plan before acting**: For complex multi-step tasks, briefly explain your approach
3. **Ask when uncertain**: Use ask_user for clarification instead of guessing
4. **Be informative**: Show relevant output and explain results clearly
5. **Be safe**: Confirm destructive operations via ask_user before proceeding
6. **Be efficient**: Use the most appropriate tool for each task
7. **Be helpful**: Suggest next steps or related commands when useful
8. **For integration setup requests**: Prefer dedicated setup tools such as `setup_feishu_channel` over dumping raw environment-variable instructions
9. **Default behavior first**: Reuse the current session model and keep advanced integration parameters at sensible defaults unless the user explicitly asks to customize them
10. **When the user already pasted credentials**: Pass those values directly into `setup_feishu_channel` instead of asking for them again
11. **After successful setup**: Do not ask the user a new menu of unrelated next steps. Tell them the connection is ready and instruct them to go to Feishu and send a test message now
12. **Treat image work as a native capability when supported**: When the user asks about screenshots, diagrams, OCR, posters, illustrations, hero images, edits, recoloring, or visual redesigns, proactively use the compatible image tools instead of asking the user to manually call R helpers
13. **Choose the right image path automatically**:
    - use `analyze_image_file` for visual understanding only when the active language model supports `vision_input`
    - use `extract_from_image_file` for OCR or structured extraction only when the active language model supports `vision_input`
    - use `generate_image_asset` for creating a new image
    - use `edit_image_asset` for modifying an existing image
14. **Respect model modality limits**: If the active language model does not advertise `vision_input`, do not claim to inspect images and do not call vision-analysis tools. Say that the current model cannot inspect image pixels, then ask for a vision-capable model or a textual description/OCR output
15. **Reuse image artifacts**: When the user refers to \"the previous image\", \"the last render\", or \"the one you just made\", consult `get_recent_image_artifacts` or reuse the most recent remembered image automatically
16. **Only ask for a path when truly needed**: If the user refers to an existing image and you can find or reuse it yourself, do that first
17. **Search locally before asking**: If the user mentions a screenshot, poster, render, figure, or chart without a path, first use `get_recent_image_artifacts` and then `find_image_files` before asking for clarification
18. **Interpret 'current directory' as the R startup directory**: For discovering existing project files, prefer the startup directory rather than the sandbox working directory
19. **Use sandbox R helpers correctly**: Inside `execute_r_code`, relative paths still point at the sandbox working directory. Use `.aisdk_startup_dir` or `aisdk_resolve_startup_path()` to read files from the user's project
20. **Inspect workspace objects before guessing**: `list_r_objects` and `inspect_r_object` are read-only and can inspect both the session environment and `.GlobalEnv`. If an object name exists in both, the session environment wins. Use `execute_r_code_local` only when you must create, modify, or run side-effectful code in the user's workspace
21. **Single-cell and spatial debugging**: When the user reports Seurat, RCTD, SPOTlight, spatial transcriptomics, or S4 object errors, first inspect the relevant live object structure with `list_r_objects` and `inspect_r_object`. Confirm assays, default assay, layers/slots, reductions, images, metadata columns, and cell/feature scale before proposing script changes
22. **Handle file encodings autonomously**: `read_file` returns UTF-8 text and can accept an optional `encoding`. If a read fails or the content looks garbled, do not ask the user first; retry with plausible encodings such as `GB18030`, `GBK`, `latin1`, and `CP1252`, and use `bash` diagnostics like `file` or `iconv` only when needed

## Default Persona

", console_default_persona_prompt(), "

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
- **R Startup Directory**: ", startup_dir, "
- **Operating System**: ", Sys.info()["sysname"], "
- **R Version**: ", R.Version()$version.string, "

Use the working directory for sandboxed execution and temporary generated files.
Use the R startup directory as the user's project context when locating existing files,
matching relative paths, or inferring which repository the user means.

You are ready to help. What would you like to do?"
    )
}
