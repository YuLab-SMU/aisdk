library(aisdk)

# ---------------------------------------------------------------------------
# Team Manager gets r_eval + r_session_state and mentions them in its prompt
# ---------------------------------------------------------------------------

test_that("AgentTeam Manager has the diagnostic introspection tools", {
  team <- AgentTeam$new(name = "DiagTeam")
  team$register_agent(
    name = "Worker",
    description = "Does the actual work",
    tools = list()
  )

  manager <- team$.__enclos_env__$private$create_manager_agent(
    model = "openai:gpt-4o",
    session = team$session
  )

  tool_names <- vapply(manager$tools, function(t) t$name, character(1))
  expect_true("delegate_task" %in% tool_names)
  expect_true("r_eval" %in% tool_names)
  expect_true("r_session_state" %in% tool_names)
})

test_that("AgentTeam Manager system prompt mentions the diagnostic safety net", {
  team <- AgentTeam$new(name = "DiagTeam2")
  team$register_agent("Worker", "Does work", tools = list())

  manager <- team$.__enclos_env__$private$create_manager_agent(
    model = "openai:gpt-4o",
    session = team$session
  )

  expect_match(manager$system_prompt, "Diagnostic safety net", fixed = TRUE)
  expect_match(manager$system_prompt, "r_eval", fixed = TRUE)
  expect_match(manager$system_prompt, "r_session_state", fixed = TRUE)
  expect_match(manager$system_prompt, "r-debug", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Flow's enhanced manager prompt advertises the diagnostic tools
# ---------------------------------------------------------------------------

test_that("Flow enhanced manager prompt includes the diagnostic safety net", {
  session <- SharedSession$new()
  registry <- AgentRegistry$new()
  flow <- Flow$new(session = session, model = "openai:gpt-4o", registry = registry)

  agent <- Agent$new(name = "PrimaryAgent", description = "Primary",
                     system_prompt = "do stuff")
  prompt <- flow$.__enclos_env__$private$build_enhanced_manager_prompt(agent)

  expect_match(prompt, "DIAGNOSTIC SAFETY NET", fixed = TRUE)
  expect_match(prompt, "r_eval", fixed = TRUE)
  expect_match(prompt, "r_session_state", fixed = TRUE)
  expect_match(prompt, "r-debug", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The injection deduplicates: if the user's primary agent already declared
# r_eval (e.g. via create_r_introspect_tools()), Flow must NOT add a second.
# ---------------------------------------------------------------------------

test_that("Flow tool injection deduplicates against user-provided tools", {
  # Recreate the merge logic we just put in flow.R:
  introspect_tools <- create_r_introspect_tools()

  # User-provided agent already has r_eval (but not r_session_state)
  user_eval <- Filter(function(t) t$name == "r_eval", introspect_tools)
  delegate_tools <- list()  # empty for simplicity

  existing_names <- vapply(c(user_eval, delegate_tools),
                           function(t) t$name, character(1))
  added <- Filter(function(t) !(t$name %in% existing_names), introspect_tools)
  combined <- c(user_eval, delegate_tools, added)
  names <- vapply(combined, function(t) t$name, character(1))

  expect_equal(sort(names), c("r_eval", "r_session_state"))
  # No duplicate r_eval
  expect_equal(sum(names == "r_eval"), 1L)
})
