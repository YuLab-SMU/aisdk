# Test Computer Abstraction Layer
# Tests for the 2026 agent design pattern: hierarchical action space

test_that("Computer class can be instantiated", {
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  expect_s3_class(comp, "R6")
  expect_equal(comp$working_dir, normalizePath(tempdir(), mustWork = FALSE))
  expect_equal(comp$sandbox_mode, "permissive")
})

test_that("Computer.bash executes simple commands", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$bash("echo 'hello world'")

  expect_false(result$error)
  expect_equal(result$exit_code, 0)
  expect_equal(trimws(result$stdout), "hello world")
})

test_that("Computer.bash handles command errors gracefully", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$bash("exit 1")

  expect_true(result$error)
  expect_equal(result$exit_code, 1)
})

test_that("Computer.bash respects strict sandbox mode", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "strict")

  # Test dangerous command blocking
  result <- comp$bash("rm -rf /")

  expect_true(result$error)
  expect_true(grepl("Sandbox violation", result$stderr))
})

test_that("Computer.read_file reads file contents", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  # Create test file
  test_file <- file.path(tempdir(), "test_read.txt")
  writeLines("test content", test_file)

  result <- comp$read_file("test_read.txt")

  expect_false(result$error)
  expect_equal(result$content, "test content")

  # Cleanup
  unlink(test_file)
})

test_that("Computer.read_file handles missing files", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$read_file("nonexistent_file.txt")

  expect_true(result$error)
  expect_true(grepl("File not found", result$message))
})

test_that("Computer.write_file writes content", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$write_file("test_write.txt", "new content")

  expect_false(result$error)
  expect_true(result$success)

  # Verify file was written
  expect_equal(readLines(file.path(tempdir(), "test_write.txt")), "new content")

  # Cleanup
  unlink(file.path(tempdir(), "test_write.txt"))
})

test_that("Computer.write_file creates parent directories", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$write_file("subdir/nested/file.txt", "content")

  expect_false(result$error)
  expect_true(file.exists(file.path(tempdir(), "subdir/nested/file.txt")))

  # Cleanup
  unlink(file.path(tempdir(), "subdir"), recursive = TRUE)
})

test_that("Computer.write_file respects strict sandbox mode", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "strict")

  # Try to write outside working directory
  result <- comp$write_file("/tmp/outside.txt", "content")

  expect_true(result$error)
  expect_true(grepl("Sandbox violation", result$message))
})

test_that("Computer.execute_r_code executes simple code", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$execute_r_code("2 + 2")

  expect_false(result$error)
  expect_true(result$result == 4)
})

test_that("Computer.execute_r_code handles errors", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  result <- comp$execute_r_code("stop('test error')")

  expect_true(result$error)
  expect_true(grepl("test error", result$message))
})

test_that("Computer.execute_r_code respects strict sandbox mode", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "strict")

  # Test dangerous function blocking
  result <- comp$execute_r_code("system('ls')")

  expect_true(result$error)
  expect_true(grepl("Sandbox violation", result$message))
})

test_that("Computer maintains execution log", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  comp$bash("echo test")
  comp$read_file(".nonexistent")
  log <- comp$get_log()

  expect_length(log, 2)
  expect_equal(log[[1]]$operation, "bash")
  expect_equal(log[[2]]$operation, "read_file")
})

test_that("Computer.clear_log clears execution log", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  comp$bash("echo test")
  comp$clear_log()
  log <- comp$get_log()

  expect_length(log, 0)
})

test_that("create_computer_tools returns list of Tools", {
  skip_on_cran()

  tools <- create_computer_tools(working_dir = tempdir())

  expect_length(tools, 4)
  expect_s3_class(tools[[1]], "R6")
  expect_equal(tools[[1]]$name, "bash")
  expect_equal(tools[[2]]$name, "read_file")
  expect_equal(tools[[3]]$name, "write_file")
  expect_equal(tools[[4]]$name, "execute_r_code")
})

test_that("computer tools have layer attribute set to 'computer'", {
  skip_on_cran()

  tools <- create_computer_tools(working_dir = tempdir())

  for (tool in tools) {
    expect_equal(tool$layer, "computer")
  }
})

test_that("computer tools are executable", {
  skip_on_cran()

  tools <- create_computer_tools(working_dir = tempdir())

  # Test bash tool
  result <- tools[[1]]$run(list(command = "echo 'test'"))
  expect_equal(trimws(result), "test")

  # Test write_file tool
  result <- tools[[3]]$run(list(path = "test.txt", content = "content"))
  expect_true(grepl("Successfully wrote", result))

  # Cleanup
  unlink(file.path(tempdir(), "test.txt"))
})

test_that("Tool class supports layer attribute", {
  t <- tool(
    name = "test_tool",
    description = "Test tool",
    parameters = z_object(
      x = z_string("test param")
    ),
    execute = function(args) args$x,
    layer = "computer"
  )

  expect_equal(t$layer, "computer")
})

test_that("Tool layer defaults to 'llm'", {
  t <- tool(
    name = "test_tool",
    description = "Test tool",
    parameters = z_object(
      x = z_string("test param")
    ),
    execute = function(args) args$x
  )

  expect_equal(t$layer, "llm")
})

test_that("Tool layer validation works", {
  expect_error(
    tool(
      name = "test_tool",
      description = "Test tool",
      parameters = z_object(x = z_string("test")),
      execute = function(args) NULL,
      layer = "invalid_layer"
    ),
    "Tool layer must be 'llm' or 'computer'"
  )
})

test_that("Computer works with relative paths", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  # Create a subdirectory
  subdir <- file.path(tempdir(), "subdir")
  dir.create(subdir)

  # Write file using relative path
  result <- comp$write_file("subdir/test.txt", "content")
  expect_false(result$error)

  # Read file using relative path
  result <- comp$read_file("subdir/test.txt")
  expect_false(result$error)
  expect_equal(result$content, "content")

  # Cleanup
  unlink(subdir, recursive = TRUE)
})

test_that("Computer.bash handles timeout", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  # Command that sleeps longer than timeout
  result <- comp$bash("sleep 5", timeout_ms = 100)

  expect_true(result$error)
})

test_that("Computer execution log includes timestamps", {
  skip_on_cran()
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")

  before <- Sys.time()
  comp$bash("echo test")
  after <- Sys.time()

  log <- comp$get_log()
  timestamp <- log[[1]]$timestamp

  expect_true(timestamp >= before)
  expect_true(timestamp <= after)
})

test_that("create_computer_tools accepts custom Computer instance", {
  skip_on_cran()

  custom_comp <- Computer$new(
    working_dir = tempdir(),
    sandbox_mode = "strict"
  )

  tools <- create_computer_tools(computer = custom_comp)

  expect_length(tools, 4)

  # Verify the tools use the strict computer instance
  result <- tools[[1]]$run(list(command = "rm -rf /"))
  expect_true(grepl("Sandbox violation", result) || grepl("Error", result))
})
