#' Genesis Plan-Script-Execute Workflow
#'
#' Implements a structured workflow:
#' 1. Plan: Create a task list in task.md
#' 2. Script: Write R code to fulfill the next step
#' 3. Execute: Run the code and update the plan
#'
#' @param task The task description
#' @param model The model to use
#' @param verbose Logical, whether to print details
#' @return The result of the execution
#' @keywords internal
genesis_do <- function(task,
                       model = "claude-3-5-sonnet-20241022",
                       verbose = FALSE) {

  # --- 1. Planning Phase ---
  if (verbose) {
    cat("=== Phase 1: Planning ===\n")
    cat("Generating structured plan...\n")
  }

  planner_agent <- Agent$new(
    name = "Planner",
    description = "Creates structured execution plans",
    system_prompt = paste(
      "You are a Planner Agent.",
      "Your goal is to break down the user's request into a concrete, sequential checklist.",
      "OUTPUT FORMAT:",
      "Create a file named 'task.md' in the artifact directory.",
      "The content MUST be a Markdown unordered list with checkboxes.",
      "Example:",
      "- [ ] Load dataset",
      "- [ ] Perform cleanups",
      "- [ ] Generate plot",
      "",
      "Keep steps granular but not excessive (3-8 steps usually).",
      "Do NOT implement the steps yet, just plan them."
    ),
    model = model,
    tools = create_artifact_tools() # Needs save_text_artifact
  )

  session <- create_shared_session(model = model)
  
  # Ensure artifact dir exists and is in session
  artifact_dir <- create_artifact_dir()
  assign(".artifact_dir", artifact_dir, envir = session$get_envir())

  # Run planner
  planner_agent$run(
    paste("Create a plan for:", task),
    session = session,
    model = model
  )

  # Verify plan exists (search recursively)
  plan_files <- list.files(artifact_dir, pattern = "^task\\.md$", recursive = TRUE, full.names = TRUE)
  
  if (length(plan_files) == 0) {
    # Fallback
    warning("Planner did not create task.md. Proceeding with direct execution.")
    return(NULL)
  }
  
  plan_path <- plan_files[1] # Take the first one if multiple

  if (verbose) {
    cat(sprintf("Plan created at: %s\n", plan_path))
    cat(readLines(plan_path), sep = "\n")
  }

  # --- 2. Execution Loop ---
  if (verbose) {
    cat("\n=== Phase 2: Execution ===\n")
  }

  worker_agent <- create_coder_agent(name = "Worker")
  # Add artifact tools to worker so it can update the plan and save scripts
  worker_agent$tools <- c(worker_agent$tools, create_artifact_tools(artifact_dir)) 
  
  # Update worker prompt to understand the workflow
  worker_agent$system_prompt <- paste(
    worker_agent$system_prompt,
    "\n\nCONTEXT:",
    "You are working on a task defined in 'task.md' in the artifact directory.",
    "Your workflow is:",
    "1. Read 'task.md' to find the next unchecked item ('- [ ]').",
    "2. Create an R script file (e.g., 'step_01_load_data.R') to implement that item.",
    "3. Execute the script using `execute_r_code` (or source it).",
    "4. If successful, update 'task.md' to mark the item as done ('- [x]').",
    "5. Repeat until all items are done.",
    sep = "\n"
  )

  # Max steps to prevent infinite loops
  max_loop_steps <- 15 
  for (i in 1:max_loop_steps) {
    
    # Check plan status
    plan_content <- readLines(plan_path)
    if (!any(grepl("- \\[ \\]", plan_content))) {
      if (verbose) cat("All tasks completed!\n")
      break
    }
    
    if (verbose) cat(sprintf("\n--- Step %d ---\n", i))
    
    # Run worker
    # We ask it to "Process the next item in the plan"
    worker_agent$run(
      "Check task.md and execute the next pending task. Remember to update task.md when done.",
      session = session,
      model = model
    )
  }

  if (verbose) {
    cat("\n=== Execution Complete ===\n")
  }
  
  # Return the session environment or a summary
  invisible(session$get_envir())
}
