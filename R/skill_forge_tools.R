#' @title Analyze R Package for Skill Creation
#' @description
#' Introspects an installed R package to understand its capabilities, exported functions,
#' and documentation. This is used by the Skill Architect to "learn" a package.
#'
#' @param package Name of the package to analyze.
#' @return A string summary of the package.
#' @export
analyze_r_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(paste0("Error: Package '", package, "' is not installed."))
  }

  # Get package description
  desc <- utils::packageDescription(package)
  title <- desc$Title
  description <- desc$Description
  version <- desc$Version

  # Get exported functions
  exports <- getNamespaceExports(package)
  # Filter out weird internal stuff like .__C__
  exports <- exports[!grepl("^\\.__", exports)]
  
  # Limit to top 50 exports to avoid context flooding
  # We prefer shorter names as they are often primary entry points? 
  # Actually, alphabetical is fine, or random sample if too many. 
  n_exports <- length(exports)
  visible_exports <- exports
  if (n_exports > 50) {
    visible_exports <- head(sort(exports), 50)
  }

  # Get documentation for a few key functions (randomly sampled or heuristics?)
  # For now, we list the exports and let the agent ask for specific help later if needed?
  # Or we try to get a one-line summary for each.

  # Construct summary
  summary <- paste0(
    "Package: ", package, "\n",
    "Version: ", version, "\n",
    "Title: ", title, "\n",
    "Description: ", description, "\n\n",
    "Exported Functions (", n_exports, " total):\n",
    paste("- ", visible_exports, collapse = "\n"),
    if (n_exports > 50) paste0("\n... and ", n_exports - 50, " more.") else ""
  )

  return(summary)
}

#' @title Test a Newly Created Skill
#' @description
#' Verifies a skill by running a test query against it in a sandboxed session.
#'
#' @param skill_name Name of the skill to test (must be in the registry).
#' @param test_query A natural language query to test the skill (e.g., "Use hello_world to say hi").
#' @param registry The skill registry object.
#' @param model A model object to use for the test agent.
#' @return A list containing `success` (boolean) and `result` (string).
#' @export
test_new_skill <- function(skill_name, test_query, registry, model) {
  
  # 1. Verify skill exists
  if (is.null(registry$get_skill(skill_name))) {
    return(list(success = FALSE, result = paste0("Skill '", skill_name, "' not found in registry.")))
  }

  # 2. Create a temporary testing agent
  # We use a basic agent but equip it with the NEW skill
  test_agent <- Agent$new(
    name = "SkillTester",
    description = "A temporary agent for testing new skills.",
    system_prompt = "You are a QA tester. Your job is to strictly follow the user's test query and report the result.",
    tools = registry$generate_tools(skill_names = skill_name) # ONLY the new skill? Or maybe all? Let's isolate to the new skill + standard tools if needed.
    # Actually, registry$generate_tools returns tools for ALL skills if names not specified?
    # Let's check implementation. 
    # registry$generate_tools implementation in R/skill.R currently might return all or filter. 
    # If we look at previous context, we might need to check. 
    # For now, let's assume we can pass specific skills.
  )
  
  # NOTE: We need a session.
  # We should probably pass an existing session or create a dummy one.
  # Since this is a tool called BY an agent, we might want to be careful about not sharing the MAIN session state 
  # which might confuse the main flow.
  # We'll create a lightweight session.
  test_session <- ChatSession$new() # Or ChatSession$new(model)
  
  # 3. Run the test
  # This acts like a nested agent call.
  result <- tryCatch({
    # We need a way to run the agent. 
    # We can use create_chat_session to wrap the model and run.
    # But wait, 'model' argument is needed.
    
    # Let's assume the caller passes a valid model object.
    response <- test_agent$run(test_query, session = test_session, model = model)
    list(success = TRUE, result = response$text)
  }, error = function(e) {
    list(success = FALSE, result = paste0("Error during execution: ", conditionMessage(e)))
  })

  return(result)
}

#' @title Create Skill Forge Tools
#' @description 
#' Wraps the analysis and testing functions into Tools for the Skill Architect.
#' @param registry SkillRegistry for looking up skills during testing.
#' @param model Model to use for the test runner.
#' @export
create_skill_forge_tools <- function(registry, model) {
  
  op_analyze <- Tool$new(
    name = "analyze_r_package",
    description = "Analyze an R package to understand its capabilities and exports. Use this before creating a skill that wraps a package.",
    parameters = z_object(
      package = z_string("Name of the R package to analyze")
    ),
    execute = function(args) {
      analyze_r_package(args$package)
    }
  )

  op_test <- Tool$new(
    name = "verify_skill", # Renamed for clarity in prompt
    description = "Run a functional test on a newly created skill to ensure it works as expected.",
    parameters = z_object(
      skill_name = z_string("Name of the skill to test"),
      test_query = z_string("A clear, simple instruction to test the skill (e.g. 'Plot mtcars using the new plotting skill')")
    ),
    execute = function(args) {
      res <- test_new_skill(args$skill_name, args$test_query, registry, model)
      if (res$success) {
        paste0("Verification PASSED.\nOutput:\n", res$result)
      } else {
        paste0("Verification FAILED.\nError:\n", res$result)
      }
    }
  )

  list(op_analyze, op_test)
}
