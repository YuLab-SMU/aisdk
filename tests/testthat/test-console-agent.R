test_that("create_console_agent creates valid agent", {
    agent <- create_console_agent()
    expect_s3_class(agent, "Agent")
    expect_equal(agent$name, "ConsoleAgent")
    expect_true(length(agent$tools) >= 12)
})

test_that("create_console_agent auto-loads local skill tools when available", {
    agent <- create_console_agent()
    tool_names <- sapply(agent$tools, function(t) t$name)

    expect_true("load_skill" %in% tool_names)
    expect_true("execute_skill_script" %in% tool_names)
})

test_that("create_console_agent accepts explicit skill roots", {
    skill_root <- tempfile("console-agent-skills-")
    dir.create(file.path(skill_root, "custom-skill"), recursive = TRUE)
    on.exit(unlink(skill_root, recursive = TRUE), add = TRUE)

    writeLines(c(
        "---",
        "name: custom-skill",
        "description: Custom console skill",
        "---",
        "Custom skill body"
    ), file.path(skill_root, "custom-skill", "SKILL.md"))

    agent <- create_console_agent(skills = skill_root)
    session <- create_chat_session(model = "mock:test", agent = agent)
    registry <- aisdk:::console_get_skill_registry(session)

    expect_true(registry$has_skill("custom-skill"))
})

test_that("create_console_agent auto-discovers skills from startup directory", {
    startup_dir <- tempfile("console-agent-startup-skills-")
    skill_dir <- file.path(startup_dir, ".skills", "startup-skill")
    dir.create(skill_dir, recursive = TRUE)
    on.exit(unlink(startup_dir, recursive = TRUE), add = TRUE)

    writeLines(c(
        "---",
        "name: startup-skill",
        "description: Startup directory skill",
        "---",
        "Startup skill body"
    ), file.path(skill_dir, "SKILL.md"))

    agent <- withr::with_dir(tempdir(), {
        create_console_agent(working_dir = tempdir(), startup_dir = startup_dir)
    })
    session <- create_chat_session(model = "mock:test", agent = agent)
    registry <- aisdk:::console_get_skill_registry(session)

    expect_true(registry$has_skill("startup-skill"))
})

test_that("console turn routing preloads explicitly referenced persona skill", {
    agent <- create_console_agent()
    session <- create_chat_session(model = "mock:test", agent = agent)

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "Y叔在吗？")

    expect_true(nzchar(routed_prompt))
    expect_true(grepl("\\[persona_begin\\]", routed_prompt))
    expect_true(grepl("colleague-yshu-code-evolution", routed_prompt, fixed = TRUE))
    expect_true(grepl("Y叔", routed_prompt, fixed = TRUE))
    expect_true(grepl("\\[reply_language_begin\\]", routed_prompt))
    expect_true(grepl("Current user language: Chinese", routed_prompt, fixed = TRUE))
    expect_true(grepl("Reply-language invariant", routed_prompt, fixed = TRUE))
})

test_that("console turn routing preloads luxun persona for @ mentions", {
    agent <- create_console_agent()
    session <- create_chat_session(model = "mock:test", agent = agent)

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "@鲁迅 教教我R语言")

    expect_true(nzchar(routed_prompt))
    expect_true(grepl("\\[persona_begin\\]", routed_prompt))
    expect_true(grepl("Active persona: 鲁迅", routed_prompt, fixed = TRUE))
    expect_true(grepl("你就是鲁迅本人", routed_prompt, fixed = TRUE))
    expect_true(grepl("luxun-perspective", routed_prompt, fixed = TRUE))
})

test_that("manual persona produces turn persona prompt without skill match", {
    session <- create_chat_session(model = "mock:test")
    aisdk:::console_set_manual_persona(
        session,
        "You are a relentlessly skeptical reviewer.",
        label = "skeptic",
        locked = TRUE
    )

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "帮我看看这个方案")

    expect_true(grepl("\\[persona_begin\\]", routed_prompt))
    expect_true(grepl("skeptic", routed_prompt, fixed = TRUE))
    expect_true(grepl("relentlessly skeptical reviewer", routed_prompt, fixed = TRUE))
    expect_true(grepl("\\[reply_language_begin\\]", routed_prompt))
    expect_true(grepl("Current user language: Chinese", routed_prompt, fixed = TRUE))
})

test_that("console turn routing injects English reply language when input is English", {
    agent <- create_console_agent()
    session <- create_chat_session(model = "mock:test", agent = agent)

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "@Guangchuang can you teach me ggtree in two sentences?")

    expect_true(nzchar(routed_prompt))
    expect_true(grepl("colleague-yshu-code-evolution", routed_prompt, fixed = TRUE))
    expect_true(grepl("\\[reply_language_begin\\]", routed_prompt))
    expect_true(grepl("Current user language: English", routed_prompt, fixed = TRUE))
    expect_true(grepl("Write the final answer in English", routed_prompt, fixed = TRUE))
    expect_true(grepl("Reply-language invariant", routed_prompt, fixed = TRUE))
    expect_true(grepl("Do not answer in Chinese", routed_prompt, fixed = TRUE))
})

test_that("console turn routing injects non-vision model limits", {
    session <- create_chat_session(model = "deepseek:deepseek-v4-flash")

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "帮我看看这个图")

    expect_true(grepl("Model registry: vision_input = false.", routed_prompt, fixed = TRUE))
    expect_true(grepl("Do not call `analyze_image_file` or `extract_from_image_file`", routed_prompt, fixed = TRUE))
    expect_true(grepl("Current user language: Chinese", routed_prompt, fixed = TRUE))
})

test_that("console turn routing respects configured vision capability model", {
    session <- create_chat_session(model = "deepseek:deepseek-v4-flash")
    session$set_capability_model("vision.inspect", "openai:gpt-4o", type = "language")

    routed_prompt <- aisdk:::console_build_turn_system_prompt(session, "帮我看看这个图")

    expect_false(grepl("Model registry: vision_input = false.", routed_prompt, fixed = TRUE))
    expect_true(grepl("Current user language: Chinese", routed_prompt, fixed = TRUE))
})

test_that("console turn routing can match custom skill by when_to_use and paths", {
    skill_root <- tempfile("console-skill-")
    dir.create(skill_root, recursive = TRUE)
    dir.create(file.path(skill_root, "withdrawal_advisor"))
    dir.create(file.path(skill_root, "cases"))
    file.create(file.path(skill_root, "cases", "student-case.md"))
    on.exit(unlink(skill_root, recursive = TRUE), add = TRUE)

    writeLines(c(
        "---",
        "name: withdrawal_advisor",
        "description: Handles withdrawal conversations",
        "when_to_use: Use this when the user says they want to drop out, withdraw, 退学, or needs emotional support about leaving school",
        "paths:",
        "  - cases/*.md",
        "---",
        "Offer practical and emotionally steady advice."
    ), file.path(skill_root, "withdrawal_advisor", "SKILL.md"))

    agent <- create_agent(
        name = "ConsoleWithCustomSkill",
        description = "Console with targeted skills",
        system_prompt = build_console_system_prompt(skill_root, skill_root, "permissive", "auto"),
        tools = create_console_tools(working_dir = skill_root, startup_dir = skill_root, sandbox_mode = "permissive"),
        skills = skill_root
    )
    session <- create_chat_session(model = "mock:test", agent = agent)
    session$set_metadata("console_startup_dir", skill_root)

    query_prompt <- aisdk:::console_build_turn_system_prompt(session, "我要退学，想聊聊后果")
    path_prompt <- withr::with_dir(skill_root, {
        aisdk:::console_build_turn_system_prompt(session, paste("请看", file.path("cases", "student-case.md")))
    })

    expect_true(grepl("withdrawal_advisor", query_prompt, fixed = TRUE))
    expect_true(grepl("withdrawal_advisor", path_prompt, fixed = TRUE))
})

test_that("console turn routing uses stored startup directory instead of current sandbox directory", {
    startup_dir <- tempfile("console-startup-")
    sandbox_dir <- tempfile("console-sandbox-")
    dir.create(startup_dir, recursive = TRUE)
    dir.create(sandbox_dir, recursive = TRUE)
    dir.create(file.path(startup_dir, "withdrawal_advisor"))
    dir.create(file.path(startup_dir, "cases"))
    file.create(file.path(startup_dir, "cases", "student-case.md"))
    on.exit(unlink(startup_dir, recursive = TRUE), add = TRUE)
    on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

    writeLines(c(
        "---",
        "name: withdrawal_advisor",
        "description: Handles withdrawal conversations",
        "when_to_use: Use this when the user says they want to drop out, withdraw, 退学, or needs emotional support about leaving school",
        "paths:",
        "  - cases/*.md",
        "---",
        "Offer practical and emotionally steady advice."
    ), file.path(startup_dir, "withdrawal_advisor", "SKILL.md"))

    agent <- create_agent(
        name = "ConsoleWithSplitDirs",
        description = "Console with separate startup and sandbox directories",
        system_prompt = build_console_system_prompt(sandbox_dir, startup_dir, "permissive", "auto"),
        tools = create_console_tools(working_dir = sandbox_dir, startup_dir = startup_dir, sandbox_mode = "permissive"),
        skills = startup_dir
    )
    session <- create_chat_session(model = "mock:test", agent = agent)
    session$merge_metadata(list(
        console_working_dir = sandbox_dir,
        console_startup_dir = startup_dir
    ))

    path_prompt <- withr::with_dir(sandbox_dir, {
        aisdk:::console_build_turn_system_prompt(session, paste("请看", file.path("cases", "student-case.md")))
    })

    expect_true(grepl("withdrawal_advisor", path_prompt, fixed = TRUE))
})

test_that("create_console_tools includes all expected tools", {
    tools <- create_console_tools()
    tool_names <- sapply(tools, function(t) t$name)

    # Check computer tools

    expect_true("bash" %in% tool_names)
    expect_true("read_file" %in% tool_names)
    expect_true("write_file" %in% tool_names)
    expect_true("execute_r_code" %in% tool_names)
    expect_true("list_r_objects" %in% tool_names)
    expect_true("inspect_r_object" %in% tool_names)
    expect_true("inspect_r_function" %in% tool_names)
    expect_true("get_r_documentation" %in% tool_names)
    expect_true("get_r_source" %in% tool_names)

    # Check console-specific tools
    expect_true("list_directory" %in% tool_names)
    expect_true("find_files" %in% tool_names)
    expect_true("find_image_files" %in% tool_names)
    expect_true("get_system_info" %in% tool_names)
    expect_true("get_environment" %in% tool_names)
    expect_true("setup_feishu_channel" %in% tool_names)
    expect_true("analyze_image_file" %in% tool_names)
    expect_true("extract_from_image_file" %in% tool_names)
    expect_true("generate_image_asset" %in% tool_names)
    expect_true("edit_image_asset" %in% tool_names)
    expect_true("get_recent_image_artifacts" %in% tool_names)
})

test_that("list_directory tool works", {
    tools <- create_console_tools()
    list_dir_tool <- tools[[which(sapply(tools, function(t) t$name) == "list_directory")]]

    result <- list_dir_tool$run(list(path = "."))
    expect_true(grepl("Directory:", result))
    expect_true(grepl("items", result))
})

test_that("console file discovery tools prefer startup directory for current project files", {
    startup_dir <- tempfile("console-startup-files-")
    sandbox_dir <- tempfile("console-sandbox-files-")
    dir.create(startup_dir, recursive = TRUE)
    dir.create(sandbox_dir, recursive = TRUE)
    writeLines("tree-content", file.path(startup_dir, "demo_tree.nwk"))
    on.exit(unlink(startup_dir, recursive = TRUE), add = TRUE)
    on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = sandbox_dir, startup_dir = startup_dir, sandbox_mode = "permissive")
    list_dir_tool <- tools[[which(sapply(tools, function(t) t$name) == "list_directory")]]
    find_tool <- tools[[which(sapply(tools, function(t) t$name) == "find_files")]]
    read_tool <- tools[[which(sapply(tools, function(t) t$name) == "read_file")]]

    listed <- list_dir_tool$run(list(path = "."))
    found <- find_tool$run(list(pattern = "*.nwk", path = ".", recursive = FALSE))
    content <- read_tool$run(list(path = "demo_tree.nwk"))

    expect_true(grepl("demo_tree.nwk", listed, fixed = TRUE))
    expect_true(grepl("demo_tree.nwk", found, fixed = TRUE))
    expect_equal(content, "tree-content")
})

test_that("read_file reports image files instead of reading binary bytes", {
    workdir <- tempfile("console-binary-read-")
    dir.create(workdir, recursive = TRUE)
    image_path <- file.path(workdir, "plot.png")
    writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x00)), image_path)
    on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = workdir, startup_dir = workdir, sandbox_mode = "permissive")
    read_tool <- tools[[which(sapply(tools, function(t) t$name) == "read_file")]]

    result <- read_tool$run(list(path = "plot.png"))

    expect_true(grepl("binary or an image", result, fixed = TRUE))
    expect_true(grepl("cannot be read as UTF-8 text", result, fixed = TRUE))
})

test_that("get_system_info tool works", {
    tools <- create_console_tools()
    sys_info_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_system_info")]]

    result <- sys_info_tool$run(list())
    expect_true(grepl("System Information", result))
    expect_true(grepl("R Version:", result))
    expect_true(grepl("Working Directory:", result))
    expect_true(grepl("Startup Directory:", result))
})

test_that("execute_r_code exposes startup directory helpers while keeping sandbox working directory", {
    startup_dir <- tempfile("console-startup-r-")
    sandbox_dir <- tempfile("console-sandbox-r-")
    dir.create(startup_dir, recursive = TRUE)
    dir.create(sandbox_dir, recursive = TRUE)
    writeLines("hello-tree", file.path(startup_dir, "demo_tree.nwk"))
    on.exit(unlink(startup_dir, recursive = TRUE), add = TRUE)
    on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = sandbox_dir, startup_dir = startup_dir, sandbox_mode = "permissive")
    exec_tool <- tools[[which(sapply(tools, function(t) t$name) == "execute_r_code")]]

    result <- exec_tool$run(list(code = paste(
        "cat(basename(getwd()), '\\n')",
        "cat(readLines(aisdk_resolve_startup_path('demo_tree.nwk')), '\\n')",
        sep = "\n"
    )))

    expect_true(grepl(basename(sandbox_dir), result, fixed = TRUE))
    expect_true(grepl("hello-tree", result, fixed = TRUE))
})

test_that("get_environment tool works", {
    tools <- create_console_tools()
    env_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_environment")]]

    result <- env_tool$run(list(names = "HOME, R_HOME"))
    expect_true(grepl("HOME=", result))
    expect_true(grepl("R_HOME=", result))
})

test_that("get_environment masks sensitive values", {
    Sys.setenv(TEST_API_KEY = "sk-1234567890abcdef")
    on.exit(Sys.unsetenv("TEST_API_KEY"))

    tools <- create_console_tools()
    env_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_environment")]]

    result <- env_tool$run(list(names = "TEST_API_KEY"))
    # Should be masked
    expect_false(grepl("sk-1234567890abcdef", result))
    expect_true(grepl("sk-1", result) || grepl("\\*\\*\\*\\*", result))
})

test_that("find_files tool works", {
    tools <- create_console_tools()
    find_tool <- tools[[which(sapply(tools, function(t) t$name) == "find_files")]]

    # Search for R files in current directory
    result <- find_tool$run(list(pattern = "*.R", path = ".", recursive = FALSE))
    # Should either find files or report "No files matching" - not an error
    expect_true(grepl("Found", result) || grepl("No files matching", result) || grepl("Directory not found", result))
})

test_that("console agent system prompt includes key elements", {
    agent <- create_console_agent()
    prompt <- agent$system_prompt

    expect_true(grepl("Terminal Assistant", prompt))
    expect_true(grepl("bash", prompt))
    expect_true(grepl("Working Directory", prompt))
    expect_true(grepl("R Startup Directory", prompt))
    expect_true(grepl("Safety", prompt))
    expect_true(grepl("setup_feishu_channel", prompt))
    expect_true(grepl("find_image_files", prompt))
    expect_true(grepl("analyze_image_file", prompt))
    expect_true(grepl("extract_from_image_file", prompt))
    expect_true(grepl("generate_image_asset", prompt))
    expect_true(grepl("edit_image_asset", prompt))
    expect_true(grepl("Treat image work as a native capability", prompt, fixed = TRUE))
    expect_true(grepl("Search locally before asking", prompt, fixed = TRUE))
    expect_true(grepl("Interpret 'current directory' as the R startup directory", prompt, fixed = TRUE))
    expect_true(grepl("Inspect workspace objects before guessing", prompt, fixed = TRUE))
    expect_true(grepl("Single-cell and spatial debugging", prompt, fixed = TRUE))
})

test_that("find_image_files ranks relevant local image candidates", {
    workdir <- tempfile("console-images-")
    dir.create(workdir, recursive = TRUE)
    file.create(file.path(workdir, "login-screenshot.png"))
    file.create(file.path(workdir, "hero-banner.jpg"))
    on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = workdir)
    find_img_tool <- tools[[which(sapply(tools, function(t) t$name) == "find_image_files")]]

    result <- find_img_tool$run(list(query = "login screenshot", path = ".", recursive = TRUE, limit = 5L))

    expect_true(grepl("Image candidates:", result, fixed = TRUE))
    expect_true(grepl("login-screenshot.png", result, fixed = TRUE))
})

test_that("analyze_image_file can auto-select a likely local image candidate", {
    workdir <- tempfile("console-images-auto-")
    dir.create(workdir, recursive = TRUE)
    file.create(file.path(workdir, "login-screenshot.png"))
    file.create(file.path(workdir, "other-banner.jpg"))
    on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = workdir)
    analyze_tool <- tools[[which(sapply(tools, function(t) t$name) == "analyze_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    local_mocked_bindings(
        analyze_image = function(model, image, prompt, ...) {
            expect_equal(image, normalizePath(file.path(workdir, "login-screenshot.png"), winslash = "/", mustWork = FALSE))
            GenerateResult$new(text = "Looks fine.")
        }
    )

    result <- analyze_tool$run(
        list(task = "Review the login screenshot"),
        envir = envir
    )

    expect_true(grepl("Looks fine.", result, fixed = TRUE))
    expect_true(any(grepl("Selection strategy:", attr(result, "aisdk_messages", exact = TRUE))))
})

test_that("analyze_image_file refuses configured non-vision current model", {
    tools <- create_console_tools()
    analyze_tool <- tools[[which(sapply(tools, function(t) t$name) == "analyze_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "deepseek:deepseek-v4-flash"

    local_mocked_bindings(
        analyze_image = function(...) {
            stop("analyze_image should not be called for a configured non-vision model")
        }
    )

    result <- analyze_tool$run(
        list(task = "Review the screenshot"),
        envir = envir
    )

    expect_true(grepl("does not advertise multimodal image input support", result, fixed = TRUE))
    expect_true(grepl("cannot inspect image pixels", result, fixed = TRUE))
})

test_that("analyze_image_file can use configured vision capability model", {
    old_routes <- aisdk:::get_capability_model_routes()
    withr::defer(aisdk:::store_capability_model_routes(old_routes))
    clear_capability_model()
    set_capability_model("vision.inspect", "openai:gpt-4o", type = "language")

    tools <- create_console_tools()
    analyze_tool <- tools[[which(sapply(tools, function(t) t$name) == "analyze_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "deepseek:deepseek-v4-flash"

    local_mocked_bindings(
        analyze_image = function(model, image, prompt, ...) {
            expect_equal(model, "openai:gpt-4o")
            expect_equal(image, "https://example.com/chart.png")
            GenerateResult$new(text = "Routed vision result.")
        }
    )

    result <- analyze_tool$run(
        list(path = "https://example.com/chart.png", task = "Read the chart"),
        envir = envir
    )

    expect_true(grepl("Routed vision result.", result, fixed = TRUE))
    expect_true(any(grepl(
        "Vision model: openai:gpt-4o",
        attr(result, "aisdk_messages", exact = TRUE),
        fixed = TRUE
    )))
})

test_that("analyze_image_file reports ambiguity when multiple candidates are similarly relevant", {
    workdir <- tempfile("console-images-ambig-")
    dir.create(workdir, recursive = TRUE)
    file.create(file.path(workdir, "screen-a.png"))
    file.create(file.path(workdir, "screen-b.png"))
    on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

    tools <- create_console_tools(working_dir = workdir)
    analyze_tool <- tools[[which(sapply(tools, function(t) t$name) == "analyze_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    result <- analyze_tool$run(
        list(task = "Check this screen"),
        envir = envir
    )

    expect_true(grepl("Multiple likely image candidates were found.", result, fixed = TRUE))
})

test_that("generate_image_asset stores recent image artifacts", {
    tools <- create_console_tools()
    gen_tool <- tools[[which(sapply(tools, function(t) t$name) == "generate_image_asset")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    local_mocked_bindings(
        generate_image = function(model, prompt, output_dir, ...) {
            expect_equal(model, "openai:gpt-image-2")
            GenerateImageResult$new(
                images = list(list(
                    path = file.path(output_dir, "generated.png"),
                    media_type = "image/png"
                )),
                text = "done"
            )
        }
    )

    result <- gen_tool$run(
        list(prompt = "Generate a blue mug image"),
        envir = envir
    )

    expect_true(grepl("Generated 1 image", result))
    expect_true(any(grepl("Image model:", attr(result, "aisdk_messages", exact = TRUE))))
    expect_equal(envir$.console_image_artifacts[[1]]$kind, "generated")
    expect_equal(envir$.console_image_artifacts[[1]]$artifacts[[1]]$path, file.path(tempdir(), "generated.png"))
    expect_match(envir$.console_image_artifacts[[1]]$artifact_id, "^img-")
})

test_that("generate_image_asset can use configured image capability model", {
    old_routes <- aisdk:::get_capability_model_routes()
    withr::defer(aisdk:::store_capability_model_routes(old_routes))
    clear_capability_model()
    set_capability_model("image.generate", "gemini:gemini-2.5-flash-image", type = "image")

    tools <- create_console_tools()
    gen_tool <- tools[[which(sapply(tools, function(t) t$name) == "generate_image_asset")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    local_mocked_bindings(
        generate_image = function(model, prompt, output_dir, ...) {
            expect_equal(model, "gemini:gemini-2.5-flash-image")
            GenerateImageResult$new(
                images = list(list(
                    path = file.path(output_dir, "generated.png"),
                    media_type = "image/png"
                ))
            )
        }
    )

    result <- gen_tool$run(
        list(prompt = "Generate a chart illustration"),
        envir = envir
    )

    expect_true(grepl("Generated 1 image", result))
    expect_equal(envir$.console_image_artifacts[[1]]$model, "gemini:gemini-2.5-flash-image")
})

test_that("edit_image_asset reuses the latest image artifact when image_path is omitted", {
    tools <- create_console_tools()
    edit_tool <- tools[[which(sapply(tools, function(t) t$name) == "edit_image_asset")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "gemini:gemini-2.5-flash"
    envir$.console_image_artifacts <- list(
        list(
            kind = "generated",
            model = "gemini:gemini-2.5-flash-image",
            prompt = "Generate a mug",
            artifacts = list(list(path = file.path(tempdir(), "last.png")))
        )
    )

    local_mocked_bindings(
        edit_image = function(model, image, prompt, mask, output_dir, ...) {
            expect_equal(model, "gemini:gemini-2.5-flash-image")
            expect_equal(image, file.path(tempdir(), "last.png"))
            expect_null(mask)
            GenerateImageResult$new(
                images = list(list(
                    path = file.path(output_dir, "edited.png"),
                    media_type = "image/png"
                ))
            )
        }
    )

    result <- edit_tool$run(
        list(prompt = "Make it cobalt blue"),
        envir = envir
    )

    expect_true(grepl("Edited 1 image", result))
    expect_true(any(grepl("Source image:", attr(result, "aisdk_messages", exact = TRUE))))
    expect_equal(envir$.console_image_artifacts[[1]]$kind, "edited")
    expect_equal(envir$.console_image_artifacts[[1]]$source_path, file.path(tempdir(), "last.png"))
})

test_that("get_recent_image_artifacts summarizes remembered image outputs", {
    tools <- create_console_tools()
    recent_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_recent_image_artifacts")]]
    envir <- new.env(parent = emptyenv())
    envir$.console_image_artifacts <- list(
        list(
            kind = "generated",
            model = "openai:gpt-image-2",
            prompt = "Generate a mug",
            artifacts = list(list(path = "/tmp/generated.png"))
        ),
        list(
            kind = "edited",
            model = "gemini:gemini-2.5-flash-image",
            prompt = "Change the color",
            artifacts = list(list(path = "/tmp/edited.png"))
        )
    )

    result <- recent_tool$run(list(limit = 2L), envir = envir)
    expect_true(grepl("Recent image artifacts:", result, fixed = TRUE))
    expect_true(grepl("/tmp/generated.png", result, fixed = TRUE))
    expect_true(grepl("/tmp/edited.png", result, fixed = TRUE))
})

test_that("extract_from_image_file returns structured text and records extraction input", {
    tools <- create_console_tools()
    extract_tool <- tools[[which(sapply(tools, function(t) t$name) == "extract_from_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    local_mocked_bindings(
        analyze_image = function(model, image, prompt, ...) {
            expect_equal(model, "openai:gpt-4o")
            expect_true(grepl("Return the result in clear JSON", prompt, fixed = TRUE))
            GenerateResult$new(text = "{\"title\":\"Mock\"}")
        }
    )

    result <- extract_tool$run(
        list(path = "https://example.com/chart.png", task = "Extract the chart title"),
        envir = envir
    )

    expect_true(grepl("\"title\":\"Mock\"", result, fixed = TRUE))
    expect_equal(envir$.console_image_artifacts[[1]]$kind, "extraction_input")
    expect_true(any(grepl("Vision model:", attr(result, "aisdk_messages", exact = TRUE))))
})

test_that("extract_from_image_file handles batch extraction paths", {
    tools <- create_console_tools()
    extract_tool <- tools[[which(sapply(tools, function(t) t$name) == "extract_from_image_file")]]
    envir <- new.env(parent = emptyenv())
    envir$.session_model_id <- "openai:gpt-4o"

    local_mocked_bindings(
        analyze_image = function(model, image, prompt, ...) {
            GenerateResult$new(text = paste0("{\"image\":\"", basename(image), "\"}"))
        }
    )

    result <- extract_tool$run(
        list(
            paths = c("https://example.com/a.png", "https://example.com/b.png"),
            task = "Extract the visible title"
        ),
        envir = envir
    )

    expect_true(grepl("a.png", result, fixed = TRUE))
    expect_true(grepl("b.png", result, fixed = TRUE))
    expect_equal(envir$.console_image_artifacts[[1]]$kind, "extraction_input")
})

test_that("setup_feishu_channel can build webhook configuration with prompt hooks", {
    menu_answers <- c(1L)
    input_answers <- c(
        "cli_test",
        "secret_test",
        tempfile(".Renviron")
    )
    saved <- NULL

    result <- setup_feishu_channel(
        prompt_hooks = list(
            menu = function(title, choices) {
                answer <- menu_answers[[1]]
                menu_answers <<- menu_answers[-1]
                answer
            },
            input = function(prompt, default = NULL) {
                answer <- input_answers[[1]]
                input_answers <<- input_answers[-1]
                answer
            },
            confirm = function(question) {
                if (grepl("advanced", question, ignore.case = TRUE)) {
                    return(FALSE)
                }
                if (grepl("Start the local Feishu webhook runtime now", question, ignore.case = TRUE)) {
                    return(FALSE)
                }
                TRUE
            },
            save = function(updates, path) {
                saved <<- list(updates = updates, path = path)
                invisible(TRUE)
            }
        ),
        current_model = "openai:gpt-4o-mini",
        workdir = tempdir(),
        session_root = file.path(tempdir(), ".aisdk", "feishu")
    )

    expect_false(isTRUE(result$cancelled))
    expect_true(result$mode %in% c("webhook", "long_connection"))
    expect_true(isTRUE(result$saved))
    expect_true(grepl("Feishu channel setup complete.", result$summary, fixed = TRUE))
    expect_equal(saved$updates$FEISHU_APP_ID, "cli_test")
    expect_equal(saved$updates$FEISHU_MODEL, "openai:gpt-4o-mini")
})

test_that("write_feishu_bridge_files copies packaged bridge assets", {
    out_dir <- tempfile("feishu-bridge-")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    info <- write_feishu_bridge_files(out_dir)

    expect_true(file.exists(file.path(out_dir, "feishu_longconn_bridge.mjs")))
    expect_true(file.exists(file.path(out_dir, "package.json")))
    expect_true(grepl("npm install", info$summary, fixed = TRUE))
})

test_that("setup_feishu_channel can consume app credentials directly", {
    saved <- NULL

    result <- setup_feishu_channel(
        prompt_hooks = list(
            menu = function(title, choices) 1L,
            input = function(prompt, default = NULL) tempfile(".Renviron"),
            confirm = function(question) {
                if (grepl("advanced", question, ignore.case = TRUE)) {
                    return(FALSE)
                }
                if (grepl("Start the local Feishu webhook runtime now", question, ignore.case = TRUE)) {
                    return(FALSE)
                }
                TRUE
            },
            save = function(updates, path) {
                saved <<- list(updates = updates, path = path)
                invisible(TRUE)
            }
        ),
        current_model = "openai:gpt-5-mini",
        app_id = "cli_a9481f474378dcb5",
        app_secret = "secret_value",
        workdir = tempdir(),
        session_root = file.path(tempdir(), ".aisdk", "feishu"),
        start_now = FALSE
    )

    expect_false(isTRUE(result$cancelled))
    expect_equal(saved$updates$FEISHU_APP_ID, "cli_a9481f474378dcb5")
    expect_equal(saved$updates$FEISHU_APP_SECRET, "secret_value")
    expect_equal(saved$updates$FEISHU_MODEL, "openai:gpt-5-mini")
})
