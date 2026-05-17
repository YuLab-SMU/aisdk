library(aisdk)

# ---------------------------------------------------------------------------
# Factory & registration
# ---------------------------------------------------------------------------

test_that("create_r_introspect_tools returns the two diagnostic tools", {
  tools <- create_r_introspect_tools()
  expect_length(tools, 2)
  names <- vapply(tools, function(t) t$name, character(1))
  expect_setequal(names, c("r_eval", "r_session_state"))
  expect_true(all(vapply(tools, function(t) inherits(t, "Tool"), logical(1))))
})

test_that("console minimal profile includes the new introspect tools", {
  tools <- create_console_tools(profile = "minimal")
  names <- vapply(tools, function(t) t$name, character(1))
  expect_true("r_eval" %in% names)
  expect_true("r_session_state" %in% names)
  # original minimal tools still present
  expect_true("bash" %in% names)
  expect_true("read_file" %in% names)
})

# ---------------------------------------------------------------------------
# r_eval
# ---------------------------------------------------------------------------

skip_if_no_callr <- function() {
  testthat::skip_if_not_installed("callr")
}

test_that("r_eval captures a simple value and reports OK", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "1 + 1"))
  expect_match(out, "status: OK", fixed = TRUE)
  expect_match(out, "[value_repr_begin]", fixed = TRUE)
  expect_match(out, "2", fixed = TRUE)
})

test_that("r_eval captures R-level errors with the error block populated", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "stop('boom')"))
  expect_match(out, "status: R_ERROR", fixed = TRUE)
  expect_match(out, "boom", fixed = TRUE)
  expect_match(out, "error_phase: eval", fixed = TRUE)
})

test_that("r_eval captures stdout and warnings separately", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "cat('hi from stdout\\n'); warning('careful'); 42"))
  expect_match(out, "hi from stdout", fixed = TRUE)
  expect_match(out, "careful", fixed = TRUE)
  expect_match(out, "[warnings_begin]", fixed = TRUE)
  # the visible value of the final 42 should appear
  expect_match(out, "42", fixed = TRUE)
})

test_that("r_eval captures subprocess stderr (critical for install-failure-style debugging)", {
  skip_if_no_callr()
  testthat::skip_on_os("windows") # /bin/sh availability
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  # Two representative paths: direct stderr write, and system() spawning a
  # grandchild that writes to stderr. The real-world install.packages /
  # processx / system() callers all use these paths.
  out <- tool$run(list(
    code = paste(
      "writeLines('from-direct-stderr', con = stderr());",
      "system('echo from-grandchild-stderr 1>&2')",
      sep = " "
    )
  ))
  expect_match(out, "from-direct-stderr", fixed = TRUE)
  expect_match(out, "from-grandchild-stderr", fixed = TRUE)
})

test_that("r_eval rejects empty code", {
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  expect_match(tool$run(list(code = "")), "non-empty", fixed = TRUE)
})

test_that("r_eval times out on long-running code without hanging", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "Sys.sleep(10)", timeout_secs = 1L))
  expect_match(out, "status: TIMEOUT", fixed = TRUE)
})

test_that("r_eval does not mutate the parent session", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  marker_name <- paste0(".aisdk_should_not_appear_", as.integer(Sys.time()))
  code <- sprintf("assign('%s', TRUE, envir = globalenv()); TRUE", marker_name)
  tool$run(list(code = code))
  expect_false(exists(marker_name, envir = globalenv(), inherits = FALSE))
})

# ---------------------------------------------------------------------------
# r_session_state
# ---------------------------------------------------------------------------

test_that("r_session_state returns the expected sections", {
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_session_state")
  out <- tool$run(list())

  expect_match(out, "[r_session_state_begin]", fixed = TRUE)
  expect_match(out, "[platform]", fixed = TRUE)
  expect_match(out, "[libpaths]", fixed = TRUE)
  expect_match(out, "[repos]", fixed = TRUE)
  expect_match(out, "[envvars]", fixed = TRUE)
  expect_match(out, "[search_path]", fixed = TRUE)
  expect_match(out, "r_version", fixed = TRUE)
})

test_that("r_session_state masks token-like env vars", {
  withr::with_envvar(c(GITHUB_PAT = "ghp_super_secret_value_1234"), {
    state <- aisdk:::collect_r_session_state(include = "envvars")
    expect_false(grepl("super_secret", state$envvars$GITHUB_PAT %||% "", fixed = TRUE))
    expect_match(state$envvars$GITHUB_PAT %||% "", "\\*\\*\\*")
  })
})

test_that("r_session_state include filter works", {
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_session_state")
  out <- tool$run(list(include = c("platform", "libpaths")))
  expect_match(out, "[platform]", fixed = TRUE)
  expect_match(out, "[libpaths]", fixed = TRUE)
  expect_false(grepl("[envvars]", out, fixed = TRUE))
  expect_false(grepl("[repos]", out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# r-debug skill discovery
# ---------------------------------------------------------------------------

test_that("r-debug skill ships with the package and exposes references", {
  skill_dir <- system.file("skills", "r-debug", package = "aisdk")
  skip_if(skill_dir == "", "r-debug skill not installed yet (run devtools::install)")
  expect_true(file.exists(file.path(skill_dir, "SKILL.md")))
  refs <- list.files(file.path(skill_dir, "references"), pattern = "\\.md$")
  expect_true(length(refs) >= 5)
  expect_true("install-failures.md" %in% refs)
})

test_that("auto skill registry picks up r-debug by name", {
  skill_dir <- system.file("skills", "r-debug", package = "aisdk")
  skip_if(skill_dir == "", "r-debug skill not installed yet (run devtools::install)")
  registry <- aisdk:::create_auto_skill_registry()
  expect_true(registry$has_skill("r-debug"))
})
