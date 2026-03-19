#' @title AI Review Panel UI
#' @description
#' Shiny module UI for reviewing AI-generated chunks.
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#' @export
aiReviewPanelUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "ai-review-panel",
    shiny::tags$div(
      class = "ai-review-panel__header",
      shiny::tags$div(
        class = "ai-review-panel__eyebrow",
        "AI Review Queue"
      ),
      shiny::tags$div(
        class = "ai-review-panel__summary",
        shiny::textOutput(ns("review_summary"), inline = TRUE)
      )
    ),
    shiny::uiOutput(ns("review_list"))
  )
}

#' @title AI Review Panel Server
#' @description
#' Shiny module server for reviewing AI-generated chunks.
#' @param id Module namespace ID.
#' @param memory ProjectMemory object or reactive returning one.
#' @param file_path Optional file path filter (reactive or static).
#' @return A list of reactive helpers for the review queue.
#' @export
aiReviewPanelServer <- function(id, memory, file_path = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh_counter <- shiny::reactiveVal(0L)
    observer_registry <- new.env(parent = emptyenv())

    resolved_memory <- shiny::reactive({
      refresh_counter()
      mem <- resolve_ai_review_panel_value(memory)
      fp <- resolve_ai_review_panel_value(file_path)

      if (is.null(mem)) {
        return(list(
          memory = NULL,
          file_path = fp,
          reviews = data.frame()
        ))
      }

      list(
        memory = mem,
        file_path = fp,
        reviews = mem$get_pending_reviews(fp)
      )
    })

    review_items <- shiny::reactive({
      resolved <- resolved_memory()
      reviews <- resolved$reviews
      mem <- resolved$memory

      if (is.null(mem) || nrow(reviews) == 0) {
        return(list())
      }

      lapply(seq_len(nrow(reviews)), function(i) {
        review <- as.list(reviews[i, , drop = FALSE])
        artifact <- mem$get_review_artifact(review$chunk_id)
        list(
          review = review,
          artifact = artifact
        )
      })
    })

    output$review_summary <- shiny::renderText({
      items <- review_items()
      if (length(items) == 0) {
        return("No pending reviews")
      }

      file_count <- length(unique(vapply(items, function(item) {
        item$review$file_path %||% ""
      }, character(1))))

      sprintf(
        "%d pending %s across %d %s",
        length(items),
        if (length(items) == 1L) "review" else "reviews",
        file_count,
        if (file_count == 1L) "file" else "files"
      )
    })

    output$review_list <- shiny::renderUI({
      items <- review_items()
      if (length(items) == 0) {
        return(shiny::p("No pending reviews"))
      }

      shiny::tagList(lapply(items, function(item) {
        render_ai_review_panel_item(
          review = item$review,
          artifact = item$artifact,
          ns = session$ns
        )
      }))
    })

    shiny::observe({
      items <- review_items()
      mem <- resolved_memory()$memory

      if (is.null(mem) || length(items) == 0) {
        return()
      }

      for (item in items) {
        chunk_id <- item$review$chunk_id
        approve_id <- paste0("approve_", chunk_id)
        reject_id <- paste0("reject_", chunk_id)

        if (!exists(approve_id, envir = observer_registry, inherits = FALSE)) {
          observer_registry[[approve_id]] <- shiny::observeEvent(input[[approve_id]], {
            mem$update_review_status(chunk_id, "approved")
            refresh_counter(refresh_counter() + 1L)
            shiny::showNotification("Chunk approved and frozen.", type = "message")
          }, ignoreInit = TRUE)
        }

        if (!exists(reject_id, envir = observer_registry, inherits = FALSE)) {
          observer_registry[[reject_id]] <- shiny::observeEvent(input[[reject_id]], {
            mem$update_review_status(chunk_id, "rejected")
            refresh_counter(refresh_counter() + 1L)
            shiny::showNotification("Chunk rejected.", type = "warning")
          }, ignoreInit = TRUE)
        }
      }
    })

    list(
      reviews_raw = shiny::reactive(resolved_memory()$reviews),
      review_items = review_items,
      refresh = function() refresh_counter(refresh_counter() + 1L)
    )
  })
}

#' @keywords internal
resolve_ai_review_panel_value <- function(x) {
  if (shiny::is.reactive(x)) {
    x()
  } else {
    x
  }
}

#' @keywords internal
derive_ai_review_panel_state <- function(review, artifact = NULL) {
  review <- review %||% list()
  artifact <- artifact %||% list()

  if (identical(review$status %||% NULL, "approved")) {
    return("frozen")
  }

  if (identical(review$status %||% NULL, "rejected")) {
    return("rejected")
  }

  if (!is.null(artifact$state) && nzchar(artifact$state)) {
    return(artifact$state)
  }

  execution_status <- review$execution_status %||% artifact$execution$status %||% NULL
  if (identical(execution_status, "completed")) {
    return("ran")
  }

  if (identical(execution_status, "error")) {
    return("error")
  }

  "draft"
}

#' @keywords internal
build_ai_review_panel_source_href <- function(file_path, chunk_label) {
  if (is.null(file_path) || is.na(file_path) || !nzchar(file_path)) {
    return(NULL)
  }

  paste0(
    "file://",
    normalizePath(file_path, winslash = "/", mustWork = FALSE),
    "#chunk=",
    utils::URLencode(chunk_label %||% "", reserved = TRUE)
  )
}

#' @keywords internal
build_ai_review_panel_toolbar <- function(review, provenance_payload, ns) {
  source_href <- build_ai_review_panel_source_href(
    file_path = review$file_path %||% NULL,
    chunk_label = review$chunk_label %||% NULL
  )

  if (is.null(source_href)) {
    return(NULL)
  }

  htmltools::tags$a(
    class = "aisdk-ai-review-card__button ai-review-panel__source-link",
    href = source_href,
    target = "_blank",
    rel = "noopener noreferrer",
    "Open Source"
  )
}

#' @keywords internal
render_ai_review_panel_item <- function(review, artifact = NULL, ns = identity) {
  review <- review %||% list()
  artifact <- artifact %||% list()

  session_summary <- compact_ai_review_value(list(
    session_id = review$session_id %||% artifact$session_id %||% NULL,
    model = artifact$model %||% NULL,
    ai_agent = review$ai_agent %||% NULL,
    uncertainty = review$uncertainty %||% NULL,
    retries = length(artifact$retries %||% list())
  ))

  provenance_payload <- build_ai_review_provenance_payload(
    chunk_id = review$chunk_id,
    chunk_label = review$chunk_label %||% "AI Chunk",
    file_path = review$file_path %||% NULL,
    artifact = utils::modifyList(
      list(
        prompt = review$prompt %||% "",
        response_text = review$response %||% "",
        final_code = review$final_code %||% artifact$final_code %||% "",
        execution = list(
          status = review$execution_status %||% artifact$execution$status %||% NULL,
          output = review$execution_output %||% artifact$execution$output %||% NULL,
          error = review$error_message %||% artifact$execution$error %||% NULL
        ),
        state = derive_ai_review_panel_state(review, artifact),
        review_mode = review$review_mode %||% artifact$review_mode %||% "required",
        runtime_mode = review$runtime_mode %||% artifact$runtime_mode %||% "static",
        session_id = review$session_id %||% artifact$session_id %||% NULL,
        model = artifact$model %||% NULL,
        retries = artifact$retries %||% list(),
        transcript = artifact$transcript %||% list()
      ),
      artifact,
      keep.null = TRUE
    ),
    review_status = review$status %||% NULL,
    timestamps = list(
      created_at = review$created_at %||% NULL,
      updated_at = review$updated_at %||% NULL,
      reviewed_at = review$reviewed_at %||% NULL
    ),
    embed_session = "full",
    ai_agent = review$ai_agent %||% NULL,
    uncertainty = review$uncertainty %||% NULL
  )

  htmltools::tags$div(
    class = "ai-review-panel__item",
    `data-chunk-id` = review$chunk_id,
    render_ai_review_card(list(
      chunk_id = review$chunk_id,
      chunk_label = review$chunk_label %||% "AI Chunk",
      state = derive_ai_review_panel_state(review, artifact),
      review = review$review_mode %||% artifact$review_mode %||% "required",
      runtime = review$runtime_mode %||% artifact$runtime_mode %||% "static",
      prompt = review$prompt %||% "",
      response = artifact$draft_response %||% review$response %||% "",
      extracted_code = review$final_code %||% artifact$final_code %||% "",
      execution_output = review$execution_output %||% artifact$execution$output %||% "",
      session_summary = if (length(session_summary) > 0) session_summary else NULL,
      provenance_payload = provenance_payload,
      hint_text = "Read-only artifact view. Inspect the stored trace and metadata, or open the source chunk directly.",
      toolbar = build_ai_review_panel_toolbar(review, provenance_payload, ns),
      timestamps = list(
        created_at = review$created_at %||% NULL,
        updated_at = review$updated_at %||% NULL,
        reviewed_at = review$reviewed_at %||% NULL
      )
    ))
  )
}
