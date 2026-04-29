# Model Registry Schema Validation Tests
# Tests for JSON schema v2 under inst/extdata/models/

# Ensure we test the current source tree, not an installed package
# When running via test_file() the installed package may be stale.
utils_models_path <- file.path("..", "..", "R", "utils_models.R")
if (file.exists(utils_models_path)) {
  source(utils_models_path, local = TRUE)
}

# ---- Helpers -----------------------------------------------------------------

.models_dir <- function() {
  # Prefer source tree during development / devtools::test()
  candidates <- c(
    "inst/extdata/models",
    file.path("..", "inst", "extdata", "models"),
    file.path("..", "..", "inst", "extdata", "models")
  )
  for (p in candidates) {
    if (dir.exists(p)) {
      return(p)
    }
  }
  # Fallback to installed package
  system.file("extdata", "models", package = "aisdk")
}

.json_files <- function() {
  d <- .models_dir()
  if (dir.exists(d)) {
    list.files(d, pattern = "\\.json$", full.names = TRUE)
  } else {
    character(0)
  }
}

.provider_name <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

.all_models <- function() {
  files <- .json_files()
  result <- list()
  for (f in files) {
    provider <- .provider_name(f)
    result[[provider]] <- jsonlite::read_json(f)
  }
  result
}

# Flatten all models into a list for vectorised checks
.flatten_models <- function() {
  by_provider <- .all_models()
  rows <- list()
  for (p in names(by_provider)) {
    for (m in by_provider[[p]]) {
      rows[[length(rows) + 1]] <- list(
        provider = p,
        model = m
      )
    }
  }
  rows
}

ALLOWED_TYPES <- c(
  "language", "chat", "embedding", "image", "audio",
  "multimodal", "video", "moderation", "rerank"
)

KNOWN_CAPABILITY_KEYS <- c(
  "reasoning", "vision_input", "image_output", "image_edit",
  "audio_input", "audio_output", "function_call",
  "structured_output", "web_search"
)

# ---- 1. JSON parsability -----------------------------------------------------

test_that("every provider JSON file parses and returns a non-empty list", {
  files <- .json_files()
  expect_true(length(files) > 0, info = "No provider JSON files found")

  for (f in files) {
    parsed <- jsonlite::read_json(f)
    expect_true(is.list(parsed),
      info = sprintf("%s should parse to a list", basename(f))
    )
    expect_true(length(parsed) > 0,
      info = sprintf("%s should contain at least one model", basename(f))
    )
  }
})

# ---- 2. ID uniqueness --------------------------------------------------------

test_that("model IDs are unique within each provider file", {
  by_provider <- .all_models()

  for (p in names(by_provider)) {
    ids <- vapply(by_provider[[p]], function(m) m$id %||% NA_character_, character(1))
    expect_equal(
      anyDuplicated(ids), 0L,
      info = sprintf("Provider '%s' contains duplicate model IDs: %s",
        p, paste(ids[duplicated(ids)], collapse = ", "))
    )
  }
})

# ---- 3. Required fields ------------------------------------------------------

test_that("every model has non-empty id and type strings", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model

    expect_true(
      is.character(m$id) && length(m$id) == 1 && nzchar(m$id),
      info = sprintf("Provider '%s' model missing valid 'id'", p)
    )
    expect_true(
      is.character(m$type) && length(m$type) == 1 && nzchar(m$type),
      info = sprintf("Provider '%s' model '%s' missing valid 'type'", p, m$id)
    )
  }
})

# ---- 4. Type whitelist -------------------------------------------------------

test_that("model type is one of the allowed values", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    expect_true(
      m$type %in% ALLOWED_TYPES,
      info = sprintf(
        "Provider '%s' model '%s' has invalid type '%s'",
        p, m$id, m$type
      )
    )
  }
})

# ---- 5. Capabilities schema --------------------------------------------------

test_that("capabilities object has only known boolean keys", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    caps <- m$capabilities

    if (is.null(caps)) {
      next
    }

    expect_true(
      is.list(caps),
      info = sprintf(
        "Provider '%s' model '%s' capabilities should be a list/object",
        p, m$id
      )
    )

    for (key in names(caps)) {
      expect_true(
        key %in% KNOWN_CAPABILITY_KEYS,
        info = sprintf(
          "Provider '%s' model '%s' has unknown capability key '%s'",
          p, m$id, key
        )
      )

      val <- caps[[key]]
      expect_true(
        is.logical(val) && length(val) == 1 && !is.na(val),
        info = sprintf(
          "Provider '%s' model '%s' capability '%s' should be boolean (not NULL/string/NA)",
          p, m$id, key
        )
      )
    }
  }
})

# ---- 6. Token validation -----------------------------------------------------

test_that("context token fields are valid positive integers when present", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    ctx <- m$context

    if (is.null(ctx)) {
      next
    }

    for (field in c("context_window", "max_input_tokens", "max_output_tokens")) {
      val <- ctx[[field]]
      if (is.null(val)) {
        next
      }

      expect_true(
        is.numeric(val) && length(val) == 1 && !is.na(val) && val > 0,
        info = sprintf(
          "Provider '%s' model '%s' %s must be a positive integer (or absent)",
          p, m$id, field
        )
      )
    }

    cw <- ctx$context_window
    mo <- ctx$max_output_tokens
    if (!is.null(cw) && !is.null(mo)) {
      expect_true(
        as.numeric(mo) <= as.numeric(cw),
        info = sprintf(
          "Provider '%s' model '%s' max_output_tokens (%s) should not exceed context_window (%s)",
          p, m$id, mo, cw
        )
      )
    }
  }
})

test_that("OpenAI model token limits match documented current values", {
  models <- .all_models()[["openai"]]
  by_id <- setNames(models, vapply(models, function(m) m$id, character(1)))

  expect_equal(by_id[["gpt-5.5"]]$context$context_window, 1050000)
  expect_equal(by_id[["gpt-5.5"]]$context$max_output_tokens, 128000)

  expect_equal(by_id[["gpt-5.4-mini"]]$context$context_window, 400000)
  expect_equal(by_id[["gpt-5.4-mini"]]$context$max_output_tokens, 128000)

  expect_equal(by_id[["gpt-5-mini"]]$context$context_window, 400000)
  expect_equal(by_id[["gpt-5-mini"]]$context$max_output_tokens, 128000)

  expect_equal(by_id[["gpt-4.1"]]$context$context_window, 1047576)
  expect_equal(by_id[["gpt-4.1"]]$context$max_output_tokens, 32768)
})

test_that("Gemini model token limits match documented current values", {
  models <- .all_models()[["gemini"]]
  by_id <- setNames(models, vapply(models, function(m) m$id, character(1)))

  expect_null(by_id[["gemini-3-pro"]])
  expect_null(by_id[["gemini-3.1-pro-preview"]])
  expect_null(by_id[["gemini-3.1-flash-lite-preview"]])

  expect_equal(by_id[["gemini-3-pro-preview"]]$context$context_window, 1048576)
  expect_equal(by_id[["gemini-3-pro-preview"]]$context$max_output_tokens, 65536)

  expect_equal(by_id[["gemini-3-flash-preview"]]$context$context_window, 1048576)
  expect_equal(by_id[["gemini-3-flash-preview"]]$context$max_output_tokens, 65536)

  expect_equal(by_id[["gemini-2.5-flash"]]$context$context_window, 1048576)
  expect_equal(by_id[["gemini-2.5-flash"]]$context$max_output_tokens, 65536)

  expect_equal(by_id[["gemini-2.0-flash"]]$context$context_window, 1048576)
  expect_equal(by_id[["gemini-2.0-flash"]]$context$max_output_tokens, 8192)
})

# ---- 7. Pricing validation ---------------------------------------------------

test_that("pricing fields are valid when present", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    price <- m$pricing

    if (is.null(price)) {
      next
    }

    for (field in c("input", "output")) {
      val <- price[[field]]
      if (is.null(val)) {
        next
      }
      expect_true(
        is.numeric(val) && length(val) == 1 && !is.na(val) && val >= 0,
        info = sprintf(
          "Provider '%s' model '%s' pricing$%s must be non-negative numeric",
          p, m$id, field
        )
      )
    }

    unit <- price$unit
    if (!is.null(unit)) {
      expect_true(
        is.character(unit) && length(unit) == 1 && nzchar(unit) &&
          grepl("tokens", unit, fixed = TRUE),
        info = sprintf(
          "Provider '%s' model '%s' pricing$unit ('%s') must contain 'tokens'",
          p, m$id, unit
        )
      )
    }
  }
})

# ---- 8. Capability consistency -----------------------------------------------

test_that("image_output=TRUE models are not solely embedding or moderation", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    caps <- m$capabilities %||% list()

    if (isTRUE(caps$image_output)) {
      expect_false(
        m$type %in% c("embedding", "moderation"),
        info = sprintf(
          "Provider '%s' model '%s' has image_output=TRUE but type='%s'",
          p, m$id, m$type
        )
      )
    }
  }
})

test_that("image_edit=TRUE implies image_output=TRUE", {
  flat <- .flatten_models()

  for (item in flat) {
    p <- item$provider
    m <- item$model
    caps <- m$capabilities %||% list()

    if (isTRUE(caps$image_edit)) {
      expect_true(
        isTRUE(caps$image_output),
        info = sprintf(
          "Provider '%s' model '%s' has image_edit=TRUE but image_output=FALSE",
          p, m$id
        )
      )
    }
  }
})

# ---- 9. list_models() integration --------------------------------------------

test_that("list_models() returns a data.frame without error", {
  df <- list_models()
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)
})

test_that("list_models() includes known provider files", {
  df <- list_models()
  known_providers <- c("deepseek", "stepfun", "volcengine")
  present_providers <- unique(df$provider)

  for (kp in known_providers) {
    expect_true(
      kp %in% present_providers,
      info = sprintf("Provider '%s' missing from list_models() output", kp)
    )
  }
})

test_that("list_models() returns expected columns", {
  df <- list_models()
  expected <- c(
    "provider", "id", "type", "family", "description",
    "reasoning", "vision_input", "image_output", "image_edit",
    "audio_input", "audio_output", "function_call", "structured_output",
    "web_search", "context_window", "max_output", "input_price", "output_price"
  )
  for (col in expected) {
    expect_true(
      col %in% names(df),
      info = sprintf("Expected column '%s' missing from list_models()", col)
    )
  }
})

test_that("list_models() has no NA in provider or id columns", {
  df <- list_models()
  expect_false(any(is.na(df$provider)),
    info = "list_models()$provider contains NA values"
  )
  expect_false(any(is.na(df$id)),
    info = "list_models()$id contains NA values"
  )
})

# ---- 10. get_model_info() integration ----------------------------------------

test_that("get_model_info returns a list for an existing model", {
  info <- get_model_info("deepseek", "deepseek-v4-flash")
  expect_true(is.list(info))
  expect_equal(info$id, "deepseek-v4-flash")
})

test_that("get_model_info returns NULL for nonexistent provider or model", {
  expect_null(get_model_info("nonexistent", "foo"))
  expect_null(get_model_info("deepseek", "nonexistent-model-id"))
})

# ---- 11. Backward compatibility ----------------------------------------------

test_that("no JSON file contains the old 'vision' capability key at top level", {
  files <- .json_files()

  for (f in files) {
    raw <- readLines(f, warn = FALSE)
    text <- paste(raw, collapse = "\n")
    # Match the exact key '"vision":' but not '"vision_input":' etc.
    has_legacy <- grepl('"vision"\\s*:', text)
    expect_false(
      has_legacy,
      info = sprintf(
        "File '%s' still contains legacy 'vision' key; migrate to 'vision_input'",
        basename(f)
      )
    )
  }
})

test_that("list_models() uses vision_input instead of legacy vision", {
  df <- list_models()
  expect_true("vision_input" %in% names(df))
  expect_type(df$vision_input, "logical")
  # Legacy 'vision' column should no longer exist
  expect_false("vision" %in% names(df))
})
