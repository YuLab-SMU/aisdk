# Tests for R-Native Programmatic Sandbox
library(testthat)
library(aisdk)

# ============================================================================
# SandboxManager: Basic Execution
# ============================================================================

test_that("SandboxManager can execute simple R code", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    result <- sandbox$execute("print(1 + 1)")
    expect_true(grepl("2", result))
})

test_that("SandboxManager captures print output", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    result <- sandbox$execute("print('hello world')")
    expect_true(grepl("hello world", result))
})

test_that("SandboxManager handles syntax errors gracefully", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    result <- sandbox$execute("if (TRUE {")
    expect_true(grepl("Error executing R code:", result))
})

test_that("SandboxManager handles runtime errors gracefully", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    result <- sandbox$execute("stop('intentional error')")
    expect_true(grepl("Error executing R code:", result))
    expect_true(grepl("intentional error", result))
})

test_that("SandboxManager handles empty code", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    result <- sandbox$execute("")
    expect_true(grepl("Error", result))
})

# ============================================================================
# SandboxManager: Variable Persistence
# ============================================================================

test_that("Variables persist across executions", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    sandbox$execute("x <- 42")
    result <- sandbox$execute("print(x)")
    expect_true(grepl("42", result))
})

test_that("Data frames persist across executions", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    sandbox$execute("df <- data.frame(a = 1:3, b = c('x', 'y', 'z'))")
    result <- sandbox$execute("print(nrow(df))")
    expect_true(grepl("3", result))
})

# ============================================================================
# SandboxManager: Tool Binding
# ============================================================================

test_that("Tool is bound and callable in sandbox", {
    mock_tool <- tool(
        name = "greet",
        description = "Greets a person",
        parameters = z_object(name = z_string("Person's name")),
        execute = function(args) paste("Hello,", args$name)
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    result <- sandbox$execute("result <- greet('Alice')\nprint(result)")
    expect_true(grepl("Hello.*Alice", result))
})

test_that("Multiple tools are bound and callable", {
    add_tool <- tool(
        name = "add_numbers",
        description = "Add two numbers",
        parameters = z_object(
            a = z_number("First number"),
            b = z_number("Second number")
        ),
        execute = function(args) args$a + args$b
    )

    multiply_tool <- tool(
        name = "multiply_numbers",
        description = "Multiply two numbers",
        parameters = z_object(
            a = z_number("First number"),
            b = z_number("Second number")
        ),
        execute = function(args) args$a * args$b
    )

    sandbox <- SandboxManager$new(
        tools = list(add_tool, multiply_tool),
        preload_packages = character(0)
    )

    result <- sandbox$execute("
    s <- add_numbers(3, 4)
    p <- multiply_numbers(3, 4)
    print(paste('Sum:', s, 'Product:', p))
  ")
    expect_true(grepl("Sum:.* 7", result))
    expect_true(grepl("Product:.* 12", result))
})

test_that("Tool results as data.frame are usable in pipeline", {
    data_tool <- tool(
        name = "get_data",
        description = "Returns sample data",
        parameters = z_object(n = z_number("Number of rows")),
        execute = function(args) {
            jsonlite::toJSON(data.frame(
                id = seq_len(args$n),
                value = seq_len(args$n) * 10
            ), auto_unbox = FALSE)
        }
    )

    sandbox <- SandboxManager$new(
        tools = list(data_tool),
        preload_packages = character(0)
    )

    result <- sandbox$execute("
    d <- get_data(5)
    print(sum(d$value))
  ")
    expect_true(grepl("150", result))
})

# ============================================================================
# SandboxManager: Batch Processing with purrr/lapply
# ============================================================================

test_that("Batch tool calls work via lapply in sandbox", {
    budget_tool <- tool(
        name = "get_budget",
        description = "Get budget for a department",
        parameters = z_object(dept = z_string("Department name")),
        execute = function(args) {
            # Simulate budget data
            budgets <- list(
                HR = list(allocated = 100, spent = 120),
                IT = list(allocated = 200, spent = 150),
                Sales = list(allocated = 150, spent = 160)
            )
            jsonlite::toJSON(budgets[[args$dept]] %||% list(allocated = 0, spent = 0),
                auto_unbox = TRUE
            )
        }
    )

    sandbox <- SandboxManager$new(
        tools = list(budget_tool),
        preload_packages = character(0)
    )

    result <- sandbox$execute("
    depts <- c('HR', 'IT', 'Sales')
    results <- lapply(depts, function(d) {
      b <- get_budget(d)
      data.frame(dept = d, allocated = b$allocated, spent = b$spent,
                 over = b$spent > b$allocated)
    })
    all_data <- do.call(rbind, results)
    over_budget <- all_data[all_data$over == TRUE, ]
    print(over_budget)
  ")
    expect_true(grepl("HR", result))
    expect_true(grepl("Sales", result))
    # IT should NOT appear (150 < 200)
    expect_false(grepl("IT", result))
})

# ============================================================================
# SandboxManager: Output Truncation
# ============================================================================

test_that("Output is truncated when exceeding max_output_chars", {
    sandbox <- SandboxManager$new(
        tools = list(),
        preload_packages = character(0),
        max_output_chars = 100
    )

    result <- sandbox$execute("print(paste(rep('aaaa', 100), collapse = ' '))")
    expect_true(grepl("truncated", result))
    # Should be close to max_output_chars but not wildly over
    expect_true(nchar(result) < 200)
})

# ============================================================================
# SandboxManager: Environment Isolation
# ============================================================================

test_that("Sandbox cannot access global environment variables", {
    # Create a variable in global env
    global_test_var_xyz <- "should not be accessible"

    sandbox <- SandboxManager$new(
        tools = list(),
        preload_packages = character(0)
    )

    result <- sandbox$execute("
    tryCatch({
      print(global_test_var_xyz)
    }, error = function(e) {
      print(paste('Isolated:', e$message))
    })
  ")
    expect_true(grepl("Isolated|not found|Error", result))

    rm(global_test_var_xyz)
})

# ============================================================================
# SandboxManager: Tool Signatures
# ============================================================================

test_that("get_tool_signatures returns formatted signatures", {
    mock_tool <- tool(
        name = "search_docs",
        description = "Search documentation by query",
        parameters = z_object(
            query = z_string("Search query string"),
            limit = z_number("Max results to return")
        ),
        execute = function(args) "results"
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    sigs <- sandbox$get_tool_signatures()
    expect_true(grepl("search_docs", sigs))
    expect_true(grepl("query", sigs))
    expect_true(grepl("limit", sigs))
    expect_true(grepl("Search documentation", sigs))
})

test_that("get_tool_signatures returns empty for no tools", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    expect_equal(sandbox$get_tool_signatures(), "")
})

# ============================================================================
# SandboxManager: Reset
# ============================================================================

test_that("reset clears user variables but keeps tools", {
    mock_tool <- tool(
        name = "ping",
        description = "Returns pong",
        execute = function() "pong"
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    sandbox$execute("user_var <- 'hello'")
    sandbox$reset()

    # User variable should be gone
    result <- sandbox$execute("
    tryCatch(print(user_var), error = function(e) print('gone'))
  ")
    expect_true(grepl("gone|not found", result))

    # But tool should still work
    result2 <- sandbox$execute("print(ping())")
    expect_true(grepl("pong", result2))
})

# ============================================================================
# SandboxManager: Preloaded Packages
# ============================================================================

test_that("dplyr functions are available when preloaded", {
    skip_if_not_installed("dplyr")

    sandbox <- SandboxManager$new(
        tools = list(),
        preload_packages = c("dplyr")
    )

    result <- sandbox$execute("
    df <- data.frame(x = 1:5, y = c(10, 20, 30, 40, 50))
    filtered <- filter(df, y > 25)
    print(nrow(filtered))
  ")
    expect_true(grepl("3", result))
})

test_that("purrr functions are available when preloaded", {
    skip_if_not_installed("purrr")

    sandbox <- SandboxManager$new(
        tools = list(),
        preload_packages = c("purrr")
    )

    result <- sandbox$execute("
    result <- map_dbl(1:5, ~ .x * 2)
    print(result)
  ")
    expect_true(grepl("2.*4.*6.*8.*10", result))
})

# ============================================================================
# create_r_code_tool
# ============================================================================

test_that("create_r_code_tool returns a valid Tool", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))
    r_tool <- create_r_code_tool(sandbox)

    expect_s3_class(r_tool, "Tool")
    expect_equal(r_tool$name, "execute_r_code")
    expect_true(grepl("execute_r_code|R code", r_tool$description, ignore.case = TRUE))
})

test_that("create_r_code_tool includes tool names in description", {
    mock_tool <- tool(
        name = "fetch_report",
        description = "Fetch a report by ID",
        parameters = z_object(id = z_string("Report ID")),
        execute = function(args) "report data"
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    r_tool <- create_r_code_tool(sandbox)
    expect_true(grepl("fetch_report", r_tool$description))
})

test_that("create_r_code_tool$run() executes code", {
    mock_tool <- tool(
        name = "double_it",
        description = "Doubles a number",
        parameters = z_object(x = z_number("Number")),
        execute = function(args) args$x * 2
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    r_tool <- create_r_code_tool(sandbox)
    result <- r_tool$run(list(code = "print(double_it(21))"))
    expect_true(grepl("42", result))
})

test_that("create_r_code_tool validates input", {
    expect_error(create_r_code_tool("not a sandbox"), "SandboxManager")
})

# ============================================================================
# create_sandbox_system_prompt
# ============================================================================

test_that("create_sandbox_system_prompt returns instructions", {
    mock_tool <- tool(
        name = "query_db",
        description = "Execute SQL query",
        parameters = z_object(sql = z_string("SQL query")),
        execute = function(args) "results"
    )

    sandbox <- SandboxManager$new(
        tools = list(mock_tool),
        preload_packages = character(0)
    )

    prompt <- create_sandbox_system_prompt(sandbox)
    expect_true(grepl("R Code Execution Environment", prompt))
    expect_true(grepl("query_db", prompt))
    expect_true(grepl("dplyr|filter|print", prompt))
})

test_that("create_sandbox_system_prompt validates input", {
    expect_error(create_sandbox_system_prompt("not a sandbox"), "SandboxManager")
})

# ============================================================================
# SandboxManager: Print Method
# ============================================================================

test_that("SandboxManager prints correctly", {
    mock_tool <- tool("t1", "Tool 1", execute = function() "ok")
    sandbox <- SandboxManager$new(tools = list(mock_tool), preload_packages = character(0))

    output <- capture.output(print(sandbox))
    expect_true(any(grepl("SandboxManager", output)))
    expect_true(any(grepl("Tools: 1", output)))
    expect_true(any(grepl("t1", output)))
})

# ============================================================================
# SandboxManager: Parent Environment Integration
# ============================================================================

test_that("Sandbox with parent_env inherits variables", {
    parent <- new.env(parent = emptyenv())
    parent$shared_data <- "from_parent"

    sandbox <- SandboxManager$new(
        tools = list(),
        preload_packages = character(0),
        parent_env = parent
    )

    result <- sandbox$execute("print(shared_data)")
    expect_true(grepl("from_parent", result))
})

# ============================================================================
# SandboxManager: list_tools
# ============================================================================

test_that("list_tools returns names of bound tools", {
    t1 <- tool("tool_a", "A", execute = function() "a")
    t2 <- tool("tool_b", "B", execute = function() "b")

    sandbox <- SandboxManager$new(
        tools = list(t1, t2),
        preload_packages = character(0)
    )

    expect_equal(sort(sandbox$list_tools()), c("tool_a", "tool_b"))
})

# ============================================================================
# SandboxManager: Self-correction scenario
# ============================================================================

test_that("Error message is clear enough for LLM self-correction", {
    sandbox <- SandboxManager$new(tools = list(), preload_packages = character(0))

    # Simulate a typical LLM mistake: wrong variable name
    result <- sandbox$execute("
    my_data <- data.frame(x = 1:3)
    print(mydata)
  ")
    expect_true(grepl("Error", result))
    expect_true(grepl("mydata|not found", result))
})
