#' @title AI Review Card Helpers
#' @description Internal helpers for rendering static-first AI review cards.
#' @name ai_review_card
NULL

#' @keywords internal
get_ai_review_asset_dir <- function() {
  installed_path <- system.file("www", package = "aisdk")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  candidates <- c(
    file.path("inst", "www"),
    file.path("..", "inst", "www"),
    file.path("..", "..", "inst", "www")
  )
  asset_dir <- candidates[file.exists(candidates)][1]
  if (length(asset_dir) == 0 || is.na(asset_dir) || !nzchar(asset_dir)) {
    rlang::abort("Unable to locate `inst/www` assets for AI review cards.")
  }

  asset_dir
}

#' @keywords internal
ai_review_html_dependency <- function() {
  htmltools::htmlDependency(
    name = "aisdk-review",
    version = "0.0.1",
    src = c(file = get_ai_review_asset_dir()),
    stylesheet = "aisdk-review.css",
    script = "aisdk-review.js"
  )
}

#' @keywords internal
render_ai_review_card_html <- function(card) {
  rendered <- htmltools::renderTags(render_ai_review_card(card))
  html <- paste(rendered$html, collapse = "")

  # When this HTML is injected into markdown as an as-is chunk result, pretty
  # printed indentation between block tags can be interpreted by Pandoc/Quarto
  # as code blocks inside the outer <div>. Compact inter-tag whitespace so the
  # full card survives as raw HTML.
  gsub(">\\s+<", "><", html, perl = TRUE)
}

#' @keywords internal
render_ai_review_card <- function(card) {
  normalized <- normalize_ai_review_card(card)
  dep <- ai_review_html_dependency()
  dependency_tags <- htmltools::singleton(
    htmltools::tags$head(
      htmltools::HTML(paste(htmltools::renderDependencies(list(dep)), collapse = "\n"))
    )
  )

  htmltools::tagList(
    dependency_tags,
    htmltools::tags$div(
      class = "aisdk-ai-review-card",
      `data-chunk-id` = normalized$chunk_id,
      `data-state` = normalized$state,
      `data-review` = normalized$review,
      `data-runtime` = normalized$runtime,
      `data-runtime-tier` = normalized$runtime_tier %||% NULL,
      `data-has-provenance` = if (!is.null(normalized$provenance_payload)) "true" else "false",
      `data-has-runtime-manifest` = if (!is.null(normalized$runtime_manifest)) "true" else "false",
      `data-live-review` = "false",
      render_ai_review_runtime_manifest_tag(normalized$chunk_id, normalized$runtime_manifest),
      render_ai_review_provenance_tag(normalized$chunk_id, normalized$provenance_payload),
      htmltools::tags$div(
        class = "aisdk-ai-review-card__header",
        htmltools::tags$div(
          class = "aisdk-ai-review-card__title-group",
          htmltools::tags$div(class = "aisdk-ai-review-card__eyebrow", "AI Review"),
          htmltools::tags$h3(
            class = "aisdk-ai-review-card__title",
            normalized$chunk_label
          )
        ),
        htmltools::tags$div(
          class = "aisdk-ai-review-card__meta",
          htmltools::tags$span(
            class = paste("aisdk-ai-review-card__badge", paste0("is-", normalized$state)),
            toupper(normalized$state)
          ),
          format_ai_review_timestamp(normalized$timestamps)
        )
      ),
      render_ai_review_section("Prompt", htmltools::tags$pre(normalized$prompt)),
      render_ai_review_section("Draft Response", as_ai_review_markup(normalized$response)),
      if (nzchar(normalized$extracted_code)) {
        render_ai_review_section(
          "Extracted Code",
          htmltools::tags$pre(
            htmltools::tags$code(
              `data-role` = "extracted-code-content",
              normalized$extracted_code
            )
          )
        )
      },
      if (nzchar(normalized$execution_output)) {
        render_ai_review_section(
          "Execution Result",
          as_ai_review_markup(normalized$execution_output)
        )
      },
      if (!is.null(normalized$session_summary)) {
        render_ai_review_section(
          "Agent Summary",
          render_ai_review_session_summary(normalized$session_summary)
        )
      },
      if (isTRUE(normalized$show_artifact_footer)) {
        htmltools::tags$div(
          class = "aisdk-ai-review-card__artifact-wrap",
          htmltools::tags$p(
            class = "aisdk-ai-review-card__artifact-note",
            normalized$artifact_note
          ),
          if (!is.null(normalized$toolbar)) {
            htmltools::tags$div(
              class = "aisdk-ai-review-card__utilities",
              normalized$toolbar
            )
          },
          if (nzchar(normalized$trace_text)) {
            htmltools::tags$details(
              class = "aisdk-ai-review-card__trace-panel",
              `data-role` = "trace-panel",
              htmltools::tags$summary("Agent Trace"),
              htmltools::tags$pre(
                `data-role` = "trace-content",
                normalized$trace_text
              )
            )
          },
          if (nzchar(normalized$metadata_text)) {
            htmltools::tags$details(
              class = "aisdk-ai-review-card__inspector-panel",
              `data-role` = "inspector-panel",
              htmltools::tags$summary("Artifact Metadata"),
              htmltools::tags$pre(
                `data-role` = "inspector-content",
                normalized$metadata_text
              )
            )
          }
        )
      }
    )
  )
}

#' @keywords internal
normalize_ai_review_card <- function(card) {
  timestamps <- card$timestamps %||% list()
  runtime_manifest <- card$runtime_manifest %||% NULL
  runtime_actions <- runtime_manifest$runtime$actions %||% list()

  list(
    chunk_id = card$chunk_id %||% "unknown-chunk",
    chunk_label = card$chunk_label %||% "AI Chunk",
    state = card$state %||% "draft",
    review = card$review %||% "none",
    runtime = card$runtime %||% "static",
    prompt = card$prompt %||% "",
    response = card$response %||% "",
    extracted_code = card$extracted_code %||% "",
    execution_output = card$execution_output %||% "",
    session_summary = card$session_summary %||% NULL,
    provenance_payload = card$provenance_payload %||% NULL,
    runtime_manifest = runtime_manifest,
    runtime_actions = runtime_actions,
    runtime_tier = runtime_manifest$runtime$tier %||% NULL,
    toolbar = card$toolbar %||% NULL,
    trace_text = format_ai_review_trace_text(card$provenance_payload %||% NULL),
    metadata_text = format_ai_review_metadata_text(
      runtime_manifest = runtime_manifest,
      provenance_payload = card$provenance_payload %||% NULL
    ),
    artifact_note = card$hint_text %||% default_ai_review_artifact_note(card$review %||% "none"),
    show_artifact_footer = !is.null(card$toolbar) ||
      !is.null(card$provenance_payload) ||
      !is.null(runtime_manifest) ||
      !identical(card$review %||% "none", "none"),
    timestamps = list(
      created_at = timestamps$created_at %||% NULL,
      updated_at = timestamps$updated_at %||% NULL,
      reviewed_at = timestamps$reviewed_at %||% NULL
    )
  )
}

#' @keywords internal
render_ai_review_section <- function(title, content) {
  htmltools::tags$section(
    class = "aisdk-ai-review-card__section",
    htmltools::tags$h4(class = "aisdk-ai-review-card__section-title", title),
    htmltools::tags$div(class = "aisdk-ai-review-card__section-body", content)
  )
}

#' @keywords internal
render_ai_review_button <- function(label, action, card) {
  action_spec <- card$runtime_actions[[action]] %||% NULL
  enabled <- isTRUE(action_spec$enabled)

  htmltools::tags$button(
    type = "button",
    class = "aisdk-ai-review-card__button",
    `data-action` = action,
    `data-chunk-id` = card$chunk_id,
    `data-enabled` = if (enabled) "true" else "false",
    title = action_spec$reason %||% NULL,
    disabled = if (!enabled) "disabled" else NULL,
    label
  )
}

#' @keywords internal
default_ai_review_artifact_note <- function(review) {
  if (identical(review, "none")) {
    return("Published AI artifact. Review interactions are disabled in this document.")
  }

  "Published AI artifact. Inspect the embedded agent trace and metadata here."
}

#' @keywords internal
format_ai_review_timestamp <- function(timestamps) {
  values <- Filter(Negate(is.null), list(
    created = timestamps$created_at,
    updated = timestamps$updated_at,
    reviewed = timestamps$reviewed_at
  ))

  if (length(values) == 0) {
    return(NULL)
  }

  htmltools::tags$div(
    class = "aisdk-ai-review-card__timestamps",
    unlist(lapply(names(values), function(name) {
      list(
        htmltools::tags$span(
          class = "aisdk-ai-review-card__timestamp",
          paste0(tools::toTitleCase(name), ": ", values[[name]])
        )
      )
    }), recursive = FALSE)
  )
}

#' @keywords internal
render_ai_review_session_summary <- function(summary) {
  pairs <- Filter(function(x) !is.null(x) && !is.na(x) && nzchar(as.character(x)), summary)
  if (length(pairs) == 0) {
    return(NULL)
  }

  htmltools::tags$dl(
    class = "aisdk-ai-review-card__session",
    unlist(lapply(names(pairs), function(name) {
      list(
        htmltools::tags$dt(tools::toTitleCase(gsub("_", " ", name))),
        htmltools::tags$dd(as.character(pairs[[name]]))
      )
    }), recursive = FALSE)
  )
}

#' @keywords internal
as_ai_review_markup <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }

  if (inherits(text, "shiny.tag") || inherits(text, "shiny.tag.list")) {
    return(text)
  }

  looks_like_html <- grepl("^\\s*<", text)
  if (looks_like_html) {
    return(htmltools::HTML(text))
  }

  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(htmltools::HTML(commonmark::markdown_html(text)))
  }

  htmltools::tags$pre(htmltools::htmlEscape(text))
}
