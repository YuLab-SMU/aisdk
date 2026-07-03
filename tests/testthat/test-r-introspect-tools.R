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

# The console minimal-profile tool set is asserted in the aisdk.console
# package (tests/testthat/test-console-integration.R).

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

test_that("r_eval accepts R calls with missing arguments", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(
    code = "df <- data.frame(a = 1:3, b = 4:6); df[1, ]"
  ))

  expect_match(out, "status: OK", fixed = TRUE)
  expect_match(out, "a b", fixed = TRUE)
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

test_that("r_eval tool schema rejects missing and empty code before execution", {
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  expect_true(isTRUE(tool$meta$validate_arguments))

  missing <- aisdk:::execute_tool_calls(
    list(list(id = "call_1", name = "r_eval", arguments = list())),
    list(tool)
  )
  empty <- aisdk:::execute_tool_calls(
    list(list(id = "call_2", name = "r_eval", arguments = list(code = ""))),
    list(tool)
  )

  expect_true(missing[[1]]$is_validation_error)
  expect_match(missing[[1]]$result, "Missing required argument `code`", fixed = TRUE)
  expect_true(empty[[1]]$is_validation_error)
  expect_match(empty[[1]]$result, "at least 1 character", fixed = TRUE)
})

test_that("r_eval times out on long-running code without hanging", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "Sys.sleep(10)", timeout_secs = 1L))
  expect_match(out, "status: TIMEOUT", fixed = TRUE)
})

test_that("r_eval caps the requested timeout at 120 seconds (issue #26)", {
  # Requesting a 10-minute timeout must not actually wait that long if the code
  # hangs. The tool reports the capped value in the result envelope so the LLM
  # can see what was applied.
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "1 + 1", timeout_secs = 600L))
  expect_match(out, "timeout_secs: 120", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Issue #26 layers A/B/C/D: structured rejection, marker, credential scrub,
# output truncation.
# ---------------------------------------------------------------------------

test_that("[A] r_eval rejects bare console_chat() at parse time (no timeout wait)", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  # Parse-time rejection returns a REJECTED status without ever launching the
  # subprocess, so it cannot wait out the timeout. We assert on the structured
  # output (a real timeout would surface a different message) rather than on
  # wall-clock elapsed time, which is unreliable on loaded CRAN build machines.
  out <- tool$run(list(code = "library(aisdk); console_chat()", timeout_secs = 30L))
  expect_match(out, "status: REJECTED", fixed = TRUE)
  expect_match(out, "rejection_kind: repl_launcher", fixed = TRUE)
  expect_match(out, "rejected_call: console_chat", fixed = TRUE)
  expect_match(out, "what_to_do_instead", fixed = TRUE)
})

test_that("[A] r_eval rejects namespace-qualified aisdk::console_chat()", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "aisdk::console_chat()", timeout_secs = 30L))
  expect_match(out, "status: REJECTED", fixed = TRUE)
  expect_match(out, "rejected_call: console_chat", fixed = TRUE)
})

test_that("[A] r_eval rejects blind interactive prompts (readline, menu)", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "x <- readline('> ')"))
  expect_match(out, "status: REJECTED", fixed = TRUE)
  expect_match(out, "rejection_kind: blind_prompt", fixed = TRUE)
  expect_match(out, "rejected_call: readline", fixed = TRUE)

  out <- tool$run(list(code = "menu(c('a','b'))"))
  expect_match(out, "rejected_call: menu", fixed = TRUE)
})

test_that("[A] readLines on a file is NOT rejected (only readLines on stdin)", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  tf <- tempfile()
  writeLines("hello", tf)
  on.exit(unlink(tf), add = TRUE)
  out <- tool$run(list(code = sprintf("readLines(%s)", deparse(tf))))
  expect_match(out, "status: OK", fixed = TRUE)
  expect_match(out, "hello", fixed = TRUE)

  out <- tool$run(list(code = "readLines(stdin())"))
  expect_match(out, "status: REJECTED", fixed = TRUE)
})

# Layer-B dynamic-dispatch rejection (console_chat's AISDK_INSIDE_R_EVAL
# self-check) is tested in the aisdk.console package, where console_chat()
# lives (tests/testthat/test-console-integration.R).

test_that("[B] subprocess sees AISDK_INSIDE_R_EVAL=1", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "Sys.getenv('AISDK_INSIDE_R_EVAL', unset = '<unset>')"))
  expect_match(out, "[1] \"1\"", fixed = TRUE)
})

test_that("[C] API keys are scrubbed from the subprocess by default", {
  skip_if_no_callr()
  withr::with_envvar(
    c(OPENAI_API_KEY = "sk-real-key-1234", FAKE_TOKEN = "tk-test-9999"),
    {
      tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
      out <- tool$run(list(
        code = "list(openai = Sys.getenv('OPENAI_API_KEY', unset = '<unset>'), tok = Sys.getenv('FAKE_TOKEN', unset = '<unset>'))"
      ))
      # The secret value must not appear anywhere in the captured output.
      # The subprocess sees the env var as set-but-empty (we use empty-string
      # override + --no-environ because callr's env arg is additive), so
      # nzchar() in the subprocess returns FALSE -- equivalent to "unset"
      # for any code that checks for a credential before using it.
      expect_false(grepl("sk-real-key-1234", out, fixed = TRUE))
      expect_false(grepl("tk-test-9999", out, fixed = TRUE))
    }
  )
})

test_that("[C] share_credentials = TRUE forwards API keys when explicitly opted in", {
  skip_if_no_callr()
  # Use a token-like name that is unlikely to live in the user's real .Renviron
  # (.Renviron values would otherwise override our withr-scoped value via R's
  # startup sequence even with share_credentials = TRUE).
  withr::with_envvar(
    c(AISDK_TEST_FAKE_TOKEN = "tok-real-1234"),
    {
      tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")

      # default: scrubbed
      scrubbed <- tool$run(list(
        code = "Sys.getenv('AISDK_TEST_FAKE_TOKEN', unset = '<unset>')"
      ))
      expect_false(grepl("tok-real-1234", scrubbed, fixed = TRUE))

      # opt-in: passes through
      shared <- tool$run(list(
        code = "Sys.getenv('AISDK_TEST_FAKE_TOKEN', unset = '<unset>')",
        share_credentials = TRUE
      ))
      expect_match(shared, "tok-real-1234", fixed = TRUE)
    }
  )
})

test_that("[C] non-sensitive env vars still pass through by default", {
  skip_if_no_callr()
  withr::with_envvar(
    c(MY_DEBUG_FLAG = "yes"),
    {
      tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
      out <- tool$run(list(code = "Sys.getenv('MY_DEBUG_FLAG', unset = '<unset>')"))
      expect_match(out, "\"yes\"", fixed = TRUE)
    }
  )
})

test_that("[D] huge stdout is truncated with a footer marker", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(
    code = "for (i in 1:5000) cat(paste0(strrep('x', 200), '\\n'))",
    timeout_secs = 15L
  ))
  expect_match(out, "stdout_truncated:", fixed = TRUE)
  expect_match(out, "original_bytes=", fixed = TRUE)
  expect_match(out, "of output truncated", fixed = TRUE)
  # formatted output should stay well under 1MB even with the truncation footer
  expect_lt(nchar(out), 600 * 1024)
})

test_that("[D] small output is not truncated", {
  skip_if_no_callr()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  out <- tool$run(list(code = "cat('hi\\n'); 1"))
  expect_false(grepl("stdout_truncated:", out, fixed = TRUE))
})

test_that("[D2] truncated stdout persists the full original to disk for grep", {
  # When output is too large for the inline envelope, the full original is
  # written to a temp file so the agent can grep / read_file it instead of
  # losing the middle. We exercise both the result envelope (file path
  # reachable) and the rendered output (path mentioned with a grep hint).
  skip_if_no_callr()
  captured <- aisdk:::r_eval_subprocess(
    code = paste(
      "for (i in 1:5000)",
      "cat(paste0(strrep('x', 200), ' line', i, '\\n'))",
      sep = " "
    ),
    timeout_secs = 15L
  )
  path <- captured$stdout_log_path
  expect_true(!is.null(path) && file.exists(path))
  # full original is on disk -- ~1MB, with all 5000 lines
  expect_gt(file.info(path)$size, 900 * 1024)
  expect_equal(length(readLines(path, warn = FALSE)), 5000L)
  # rendered envelope mentions the path AND tells the agent to grep instead of
  # re-running r_eval
  rendered <- aisdk:::format_r_eval_result(captured, "x", 15L)
  expect_match(rendered, "Full original output is saved at:", fixed = TRUE)
  expect_match(rendered, path, fixed = TRUE)
  expect_match(rendered, "grep -n", fixed = TRUE)
  expect_match(rendered, "Do NOT retry r_eval", fixed = TRUE)
  unlink(path)
})

test_that("[D2] non-truncated runs do NOT leave a log file behind", {
  skip_if_no_callr()
  captured <- aisdk:::r_eval_subprocess("cat('hi\\n'); 1", timeout_secs = 5L)
  expect_null(captured$stdout_log_path)
  expect_null(captured$stderr_log_path)
})

test_that("r_eval kills the whole process tree on timeout (no orphaned grandchildren)", {
  skip_if_no_callr()
  testthat::skip_on_os("windows") # uses /bin/sh + pgrep
  skip_if(Sys.which("pgrep") == "", "pgrep not available")
  # Process-tree teardown semantics belong to processx/callr (kill_tree)
  # and to the kernel's reparent-to-init behavior, not to our code. On
  # GitHub Actions Linux runners and CRAN's Debian check farm, sh+sleep
  # grandchildren sporadically get adopted by PID 1 before kill_tree can
  # reach them, even with multi-second polling. Skip in CI and on CRAN
  # so we are not asserting on an upstream guarantee we do not own. The
  # TIMEOUT status path -- which IS our responsibility and the actual
  # user-visible behavior of issue #26 -- is covered by the "r_eval times
  # out on long-running code without hanging" test above.
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  tool <- aisdk:::find_tool(create_r_introspect_tools(), "r_eval")
  # Use a unique marker that does not appear in any common command string.
  marker <- paste0("aisdk_orphan_marker_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  cmd <- sprintf("sleep 30 # %s", marker)
  out <- tool$run(list(
    code = sprintf("system(\"%s\")", cmd),
    timeout_secs = 1L
  ))
  expect_match(out, "status: TIMEOUT", fixed = TRUE)

  # Reaping the orphan tree is asynchronous: poll up to ~8s on developer
  # machines (macOS reaps in milliseconds; a loaded host can take seconds).
  no_match <- FALSE
  for (i in seq_len(80)) {
    Sys.sleep(0.1)
    pgrep_out <- suppressWarnings(system2(
      "pgrep", c("-f", marker), stdout = TRUE, stderr = FALSE
    ))
    if (length(pgrep_out) == 0) {
      no_match <- TRUE
      break
    }
  }
  expect_true(no_match, info = "process tree was not fully reaped within 8s")
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
