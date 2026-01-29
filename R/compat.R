#' @title Compatibility Layer: Feature Flags and Migration Support
#' @description
#' Provides feature flags, compatibility shims, and migration utilities
#' for controlled breaking changes in the agent SDK.
#' @name compat
NULL

# =============================================================================
# Feature Flags
# =============================================================================

#' @title SDK Feature Flags
#' @description
#' Global feature flags for controlling SDK behavior. These flags allow
#' gradual migration to new features while maintaining backward compatibility.
#' @keywords internal
.sdk_features <- new.env(parent = emptyenv())

# Default feature flag values
.sdk_features$use_shared_session <- TRUE
.sdk_features$use_flow_stack <- TRUE
.sdk_features$use_unified_delegate <- TRUE
.sdk_features$use_enhanced_agents <- TRUE
.sdk_features$strict_sandbox <- TRUE
.sdk_features$enable_tracing <- TRUE
.sdk_features$legacy_tool_format <- FALSE
.sdk_features$legacy_session_api <- FALSE

#' @title Get Feature Flag
#' @description
#' Get the current value of a feature flag.
#' @param flag Name of the feature flag.
#' @param default Default value if flag not set.
#' @return The flag value.
#' @export
#' @examples
#' \dontrun{
#' # Check if shared session is enabled
#' if (sdk_feature("use_shared_session")) {
#'   session <- create_shared_session(model = "openai:gpt-4o")
#' } else {
#'   session <- create_chat_session(model = "openai:gpt-4o")
#' }
#' }
sdk_feature <- function(flag, default = NULL) {
  if (exists(flag, envir = .sdk_features, inherits = FALSE)) {
    get(flag, envir = .sdk_features)
  } else {
    default
  }
}

#' @title Set Feature Flag
#' @description
#' Set a feature flag value. Use this to enable/disable SDK features.
#' @param flag Name of the feature flag.
#' @param value Value to set.
#' @return Invisible previous value.
#' @export
#' @examples
#' \dontrun{
#' # Disable shared session for legacy compatibility
#' sdk_set_feature("use_shared_session", FALSE)
#'
#' # Enable legacy tool format
#' sdk_set_feature("legacy_tool_format", TRUE)
#' }
sdk_set_feature <- function(flag, value) {
  old_value <- sdk_feature(flag)
  assign(flag, value, envir = .sdk_features)
  invisible(old_value)
}

#' @title List Feature Flags
#' @description
#' List all available feature flags and their current values.
#' @return A named list of feature flags.
#' @export
#' @examples
#' \dontrun{
#' # See all feature flags
#' print(sdk_list_features())
#' }
sdk_list_features <- function() {
  flags <- ls(.sdk_features)
  values <- lapply(flags, function(f) get(f, envir = .sdk_features))
  names(values) <- flags
  values
}

#' @title Reset Feature Flags
#' @description
#' Reset all feature flags to their default values.
#' @return Invisible NULL.
#' @export
sdk_reset_features <- function() {
  .sdk_features$use_shared_session <- TRUE
  .sdk_features$use_flow_stack <- TRUE
  .sdk_features$use_unified_delegate <- TRUE
  .sdk_features$use_enhanced_agents <- TRUE
  .sdk_features$strict_sandbox <- TRUE
  .sdk_features$enable_tracing <- TRUE
  .sdk_features$legacy_tool_format <- FALSE
  .sdk_features$legacy_session_api <- FALSE
  invisible(NULL)
}

# =============================================================================
# Compatibility Shims
# =============================================================================

#' @title Create Session (Compatibility Wrapper)
#' @description
#' Creates a session using either the new SharedSession or legacy ChatSession
#' based on feature flags. This provides a migration path for existing code.
#' @param model A LanguageModelV1 object or model string ID.
#' @param system_prompt Optional system prompt.
#' @param tools Optional list of Tool objects.
#' @param hooks Optional HookHandler object.
#' @param max_steps Maximum tool execution steps. Default 10.
#' @param ... Additional arguments passed to session constructor.
#' @return A SharedSession or ChatSession object.
#' @export
#' @examples
#' \dontrun{
#' # Automatically uses SharedSession if feature enabled
#' session <- create_session(model = "openai:gpt-4o")
#'
#' # Force legacy session
#' sdk_set_feature("use_shared_session", FALSE)
#' session <- create_session(model = "openai:gpt-4o")
#' }
create_session <- function(model = NULL,
                            system_prompt = NULL,
                            tools = NULL,
                            hooks = NULL,
                            max_steps = 10,
                            ...) {
  if (sdk_feature("use_shared_session", TRUE)) {
    create_shared_session(
      model = model,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks,
      max_steps = max_steps,
      sandbox_mode = if (sdk_feature("strict_sandbox", TRUE)) "strict" else "permissive",
      trace_enabled = sdk_feature("enable_tracing", TRUE),
      ...
    )
  } else {
    create_chat_session(
      model = model,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks,
      max_steps = max_steps
    )
  }
}

#' @title Create Orchestration Flow (Compatibility Wrapper)
#' @description
#' Creates an orchestration flow using either FlowStack or legacy Flow
#' based on feature flags.
#' @param session A session object.
#' @param model The default model ID.
#' @param registry Optional AgentRegistry.
#' @param max_depth Maximum delegation depth. Default 5.
#' @param max_steps_per_agent Maximum ReAct steps per agent. Default 10.
#' @param ... Additional arguments.
#' @return A FlowStack or Flow object.
#' @export
create_orchestration <- function(session,
                                  model,
                                  registry = NULL,
                                  max_depth = 5,
                                  max_steps_per_agent = 10,
                                  ...) {
  if (sdk_feature("use_flow_stack", TRUE)) {
    create_flow_stack(
      session = session,
      model = model,
      registry = registry,
      max_depth = max_depth,
      max_steps_per_agent = max_steps_per_agent,
      enable_guardrails = TRUE,
      ...
    )
  } else {
    create_flow(
      session = session,
      model = model,
      registry = registry,
      max_depth = max_depth,
      max_steps_per_agent = max_steps_per_agent
    )
  }
}

#' @title Create Standard Agent Registry
#' @description
#' Creates an AgentRegistry pre-populated with standard library agents
#' based on feature flags.
#' @param include_data Include DataAgent. Default TRUE.
#' @param include_file Include FileAgent. Default TRUE.
#' @param include_env Include EnvAgent. Default TRUE.
#' @param include_coder Include CoderAgent. Default TRUE.
#' @param include_visualizer Include VisualizerAgent. Default TRUE.
#' @param include_planner Include PlannerAgent. Default TRUE.
#' @param file_allowed_dirs Allowed directories for FileAgent.
#' @param env_allow_install Allow package installation for EnvAgent.
#' @return An AgentRegistry object.
#' @export
#' @examples
#' \dontrun{
#' # Create registry with all standard agents
#' registry <- create_standard_registry()
#'
#' # Create registry with only data and visualization agents
#' registry <- create_standard_registry(
#'   include_file = FALSE,
#'   include_env = FALSE,
#'   include_planner = FALSE
#' )
#' }
create_standard_registry <- function(include_data = TRUE,
                                      include_file = TRUE,
                                      include_env = TRUE,
                                      include_coder = TRUE,
                                      include_visualizer = TRUE,
                                      include_planner = TRUE,
                                      file_allowed_dirs = ".",
                                      env_allow_install = FALSE) {
  agents <- list()

  if (include_data && sdk_feature("use_enhanced_agents", TRUE)) {
    agents <- c(agents, list(create_data_agent()))
  }

  if (include_file && sdk_feature("use_enhanced_agents", TRUE)) {
    agents <- c(agents, list(create_file_agent(allowed_dirs = file_allowed_dirs)))
  }

  if (include_env && sdk_feature("use_enhanced_agents", TRUE)) {
    agents <- c(agents, list(create_env_agent(allow_install = env_allow_install)))
  }

  if (include_coder) {
    agents <- c(agents, list(create_coder_agent(
      safe_mode = sdk_feature("strict_sandbox", TRUE)
    )))
  }

  if (include_visualizer) {
    agents <- c(agents, list(create_visualizer_agent()))
  }

  if (include_planner) {
    agents <- c(agents, list(create_planner_agent()))
  }

  create_agent_registry(agents)
}

# =============================================================================
# Migration Utilities
# =============================================================================

#' @title Check SDK Version Compatibility
#' @description
#' Check if code is compatible with the current SDK version and
#' suggest migration steps if needed.
#' @param code_version Version string the code was written for.
#' @return A list with compatible (logical) and suggestions (character vector).
#' @export
#' @examples
#' \dontrun{
#' result <- check_sdk_compatibility("0.8.0")
#' if (!result$compatible) {
#'   cat("Migration needed:\n")
#'   cat(paste(result$suggestions, collapse = "\n"))
#' }
#' }
check_sdk_compatibility <- function(code_version) {
  current_version <- "1.0.0"  # SDK version

  suggestions <- character(0)
  compatible <- TRUE

  # Parse versions
  code_parts <- as.numeric(strsplit(code_version, "\\.")[[1]])
  current_parts <- as.numeric(strsplit(current_version, "\\.")[[1]])

  # Check major version

  if (code_parts[1] < current_parts[1]) {
    compatible <- FALSE
    suggestions <- c(suggestions,
      "Major version upgrade detected. Review breaking changes:",
      "- ChatSession -> SharedSession (use create_session() for compatibility)",
      "- Flow -> FlowStack (use create_orchestration() for compatibility)",
      "- New standard agents available (DataAgent, FileAgent, EnvAgent)",
      "- Tool execution now supports sandboxing"
    )
  }

  # Check minor version
  if (length(code_parts) >= 2 && code_parts[2] < 9) {
    suggestions <- c(suggestions,
      "Consider using new features:",
      "- Unified delegate_task tool for cleaner orchestration",
      "- Enhanced tracing and observability",
      "- Variable scoping in SharedSession"
    )
  }

  list(
    compatible = compatible,
    current_version = current_version,
    code_version = code_version,
    suggestions = suggestions
  )
}

#' @title Migrate Legacy Code
#' @description
#' Provides migration guidance for legacy code patterns.
#' @param pattern The legacy pattern to migrate from.
#' @return A list with old_pattern, new_pattern, and example.
#' @export
#' @examples
#' \dontrun{
#' # Get migration guidance for ChatSession
#' guidance <- migrate_pattern("ChatSession")
#' cat(guidance$example)
#' }
migrate_pattern <- function(pattern) {
  migrations <- list(
    "ChatSession" = list(
      old_pattern = "ChatSession$new(...)",
      new_pattern = "create_session(...) or create_shared_session(...)",
      example = paste0(
        "# Old:\n",
        "session <- ChatSession$new(model = 'openai:gpt-4o')\n\n",
        "# New (recommended):\n",
        "session <- create_session(model = 'openai:gpt-4o')\n\n",
        "# Or explicitly:\n",
        "session <- create_shared_session(\n",
        "  model = 'openai:gpt-4o',\n",
        "  sandbox_mode = 'strict',\n",
        "  trace_enabled = TRUE\n",
        ")"
      )
    ),
    "Flow" = list(
      old_pattern = "Flow$new(...) or create_flow(...)",
      new_pattern = "create_orchestration(...) or create_flow_stack(...)",
      example = paste0(
        "# Old:\n",
        "flow <- create_flow(session, model, registry)\n\n",
        "# New (recommended):\n",
        "flow <- create_orchestration(session, model, registry)\n\n",
        "# Or explicitly:\n",
        "flow <- create_flow_stack(\n",
        "  session = session,\n",
        "  model = model,\n",
        "  registry = registry,\n",
        "  enable_guardrails = TRUE\n",
        ")"
      )
    ),
    "delegate_tools" = list(
      old_pattern = "registry$generate_delegate_tools(...)",
      new_pattern = "flow$generate_delegate_tool() (unified)",
      example = paste0(
        "# Old (multiple tools):\n",
        "tools <- registry$generate_delegate_tools(flow, session, model)\n\n",
        "# New (single unified tool):\n",
        "delegate_tool <- flow$generate_delegate_tool()\n\n",
        "# The unified tool handles all agents:\n",
        "# delegate_task(agent_name = 'DataAgent', task = '...')"
      )
    ),
    "agent_library" = list(
      old_pattern = "create_coder_agent(), create_visualizer_agent()",
      new_pattern = "create_standard_registry() or individual enhanced agents",
      example = paste0(
        "# Old:\n",
        "coder <- create_coder_agent()\n",
        "viz <- create_visualizer_agent()\n",
        "registry <- create_agent_registry(list(coder, viz))\n\n",
        "# New (recommended):\n",
        "registry <- create_standard_registry(\n",
        "  include_data = TRUE,\n",
        "  include_file = TRUE,\n",
        "  include_env = TRUE\n",
        ")\n\n",
        "# New agents available:\n",
        "# - DataAgent: dplyr/tidyr operations\n",
        "# - FileAgent: file I/O with safety\n",
        "# - EnvAgent: package management"
      )
    ),
    "session_envir" = list(
      old_pattern = "session$get_envir(), session$eval_in_session()",
      new_pattern = "session$get_var(), session$set_var(), session$execute_code()",
      example = paste0(
        "# Old:\n",
        "env <- session$get_envir()\n",
        "env$my_data <- data.frame(x = 1:10)\n",
        "result <- session$eval_in_session(quote(mean(my_data$x)))\n\n",
        "# New (SharedSession):\n",
        "session$set_var('my_data', data.frame(x = 1:10))\n",
        "result <- session$execute_code('mean(my_data$x)')\n\n",
        "# With scoping:\n",
        "session$set_var('temp', 42, scope = 'agent_scope')\n",
        "val <- session$get_var('temp', scope = 'agent_scope')"
      )
    )
  )

  if (pattern %in% names(migrations)) {
    migrations[[pattern]]
  } else {
    list(
      old_pattern = pattern,
      new_pattern = "Unknown pattern",
      example = paste0(
        "Pattern '", pattern, "' not found in migration guide.\n",
        "Available patterns: ", paste(names(migrations), collapse = ", ")
      )
    )
  }
}

#' @title Print Migration Guide
#' @description
#' Print a comprehensive migration guide for upgrading to the new SDK version.
#' @param verbose Include detailed examples. Default TRUE.
#' @return Invisible NULL (prints to console).
#' @export
print_migration_guide <- function(verbose = TRUE) {
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("R AI SDK Migration Guide: v0.x -> v1.0\n")
  cat("=", rep("=", 60), "\n\n", sep = "")

  cat("OVERVIEW\n")
  cat("-", rep("-", 40), "\n", sep = "")
  cat("This version introduces:\n")
  cat("- SharedSession: Enhanced session with sandboxing and tracing\n")
  cat("- FlowStack: Improved orchestration with guardrails\n")
  cat("- Standard Agents: DataAgent, FileAgent, EnvAgent\n")
  cat("- Unified delegate_task tool\n")
  cat("- Feature flags for gradual migration\n\n")

  cat("QUICK START\n")
  cat("-", rep("-", 40), "\n", sep = "")
  cat("# Use compatibility wrappers for easy migration:\n")
  cat("session <- create_session(model = 'openai:gpt-4o')\n")
  cat("registry <- create_standard_registry()\n")
  cat("flow <- create_orchestration(session, model, registry)\n\n")

  cat("FEATURE FLAGS\n")
  cat("-", rep("-", 40), "\n", sep = "")
  cat("# Disable new features for legacy compatibility:\n")
  cat("sdk_set_feature('use_shared_session', FALSE)\n")
  cat("sdk_set_feature('use_flow_stack', FALSE)\n")
  cat("sdk_set_feature('use_enhanced_agents', FALSE)\n\n")

  if (verbose) {
    cat("DETAILED MIGRATIONS\n")
    cat("-", rep("-", 40), "\n", sep = "")

    patterns <- c("ChatSession", "Flow", "delegate_tools", "agent_library", "session_envir")
    for (p in patterns) {
      guidance <- migrate_pattern(p)
      cat("\n### ", p, " ###\n", sep = "")
      cat("Old: ", guidance$old_pattern, "\n", sep = "")
      cat("New: ", guidance$new_pattern, "\n", sep = "")
      cat("\nExample:\n")
      cat(guidance$example, "\n")
    }
  }

  cat("\n", "=", rep("=", 60), "\n", sep = "")
  cat("For more information, see the package vignettes.\n")

  invisible(NULL)
}

# =============================================================================
# Deprecation Warnings
# =============================================================================

#' @title Deprecation Warning Helper
#' @description
#' Issue a deprecation warning with migration guidance.
#' @param old_fn Name of the deprecated function/pattern.
#' @param new_fn Name of the replacement.
#' @param version Version when it will be removed.
#' @keywords internal
deprecation_warning <- function(old_fn, new_fn, version = "2.0.0") {
  msg <- sprintf(
    "'%s' is deprecated and will be removed in version %s. Use '%s' instead.",
    old_fn, version, new_fn
  )
  rlang::warn(msg, .frequency = "once", .frequency_id = old_fn)
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
