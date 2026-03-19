#' @title Project Memory System
#' @description
#' Long-term memory storage for AI agents using SQLite. Stores successful
#' code snippets, error fixes, and execution history for RAG (Retrieval
#' Augmented Generation) and learning from past interactions.
#' @name project_memory
NULL

#' @title Project Memory Class
#' @description
#' R6 class for managing persistent project memory using SQLite.
#' Stores code snippets, error fixes, and execution graphs for
#' resuming failed long-running jobs.
#' @export
ProjectMemory <- R6::R6Class(
  "ProjectMemory",
  public = list(
    #' @field db_path Path to the SQLite database file.
    db_path = NULL,

    #' @field project_root Root directory of the project.
    project_root = NULL,

    #' @description
    #' Create or connect to a project memory database.
    #' @param project_root Project root directory. Defaults to getwd() interactively, tempdir() otherwise.
    #' @param db_name Database filename. Defaults to "memory.sqlite".
    #' @return A new ProjectMemory object.
    initialize = function(project_root = if (interactive()) getwd() else tempdir(), db_name = "memory.sqlite") {
      self$project_root <- normalizePath(project_root, mustWork = TRUE)

      # Create .aisdk directory if it doesn't exist
      aisdk_dir <- file.path(self$project_root, ".aisdk")
      if (!dir.exists(aisdk_dir)) {
        dir.create(aisdk_dir, recursive = TRUE)
      }

      self$db_path <- file.path(aisdk_dir, db_name)

      # Initialize database
      private$init_db()

      invisible(self)
    },

    #' @description
    #' Store a successful code snippet for future reference.
    #' @param code The R code that was executed successfully.
    #' @param description Optional description of what the code does.
    #' @param tags Optional character vector of tags for categorization.
    #' @param context Optional context about when/why this code was used.
    #' @return The ID of the stored snippet.
    store_snippet = function(code, description = NULL, tags = NULL, context = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # Generate embedding for similarity search (simple hash for now)
      code_hash <- digest::digest(code, algo = "md5")

      # Store the snippet
      DBI::dbExecute(con, "
        INSERT INTO snippets (code, description, tags, context, code_hash, created_at)
        VALUES (?, ?, ?, ?, ?, datetime('now'))
      ", params = list(
        code,
        description,
        if (!is.null(tags)) paste(tags, collapse = ",") else NULL,
        context,
        code_hash
      ))

      DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    },

    #' @description
    #' Store an error fix for learning.
    #' @param original_code The code that produced the error.
    #' @param error The error message.
    #' @param fixed_code The corrected code.
    #' @param fix_description Description of what was fixed.
    #' @return The ID of the stored fix.
    store_fix = function(original_code, error, fixed_code, fix_description = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # Generate error signature for matching similar errors
      error_signature <- private$generate_error_signature(error)

      DBI::dbExecute(con, "
        INSERT INTO fixes (original_code, error_message, error_signature, fixed_code, fix_description, created_at)
        VALUES (?, ?, ?, ?, ?, datetime('now'))
      ", params = list(
        original_code,
        error,
        error_signature,
        fixed_code,
        fix_description
      ))

      DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    },

    #' @description
    #' Find a similar fix from memory.
    #' @param error The error message to match.
    #' @return A list with the fix details, or NULL if not found.
    find_similar_fix = function(error) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      error_signature <- private$generate_error_signature(error)

      result <- DBI::dbGetQuery(con, "
        SELECT * FROM fixes
        WHERE error_signature = ?
        ORDER BY created_at DESC
        LIMIT 1
      ", params = list(error_signature))

      if (nrow(result) == 0) {
        # Try fuzzy matching on error message
        result <- DBI::dbGetQuery(con, "
          SELECT * FROM fixes
          WHERE error_message LIKE ?
          ORDER BY created_at DESC
          LIMIT 1
        ", params = list(paste0("%", substr(error, 1, 50), "%")))
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      as.list(result[1, ])
    },

    #' @description
    #' Search for relevant code snippets.
    #' @param query Search query (matches description, tags, or code).
    #' @param limit Maximum number of results.
    #' @return A data frame of matching snippets.
    search_snippets = function(query, limit = 10) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      search_pattern <- paste0("%", query, "%")

      DBI::dbGetQuery(con, "
        SELECT id, code, description, tags, context, created_at
        FROM snippets
        WHERE code LIKE ? OR description LIKE ? OR tags LIKE ?
        ORDER BY created_at DESC
        LIMIT ?
      ", params = list(search_pattern, search_pattern, search_pattern, limit))
    },

    #' @description
    #' Store execution graph node for workflow persistence.
    #' @param workflow_id Unique identifier for the workflow.
    #' @param node_id Unique identifier for this node.
    #' @param node_type Type of node (e.g., "transform", "model", "output").
    #' @param code The code for this node.
    #' @param status Node status ("pending", "running", "completed", "failed").
    #' @param result Optional serialized result.
    #' @param dependencies Character vector of node IDs this depends on.
    #' @return The database row ID.
    store_workflow_node = function(workflow_id, node_id, node_type, code,
                                   status = "pending", result = NULL,
                                   dependencies = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # Serialize result if provided
      result_blob <- if (!is.null(result)) {
        serialize(result, NULL)
      } else {
        NULL
      }

      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO workflow_nodes
        (workflow_id, node_id, node_type, code, status, result, dependencies, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, datetime('now'))
      ", params = list(
        workflow_id,
        node_id,
        node_type,
        code,
        status,
        result_blob,
        if (!is.null(dependencies)) paste(dependencies, collapse = ",") else NULL
      ))

      DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    },

    #' @description
    #' Update workflow node status.
    #' @param workflow_id Workflow identifier.
    #' @param node_id Node identifier.
    #' @param status New status.
    #' @param result Optional result to store.
    update_node_status = function(workflow_id, node_id, status, result = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      result_blob <- if (!is.null(result)) {
        serialize(result, NULL)
      } else {
        NULL
      }

      if (!is.null(result_blob)) {
        DBI::dbExecute(con, "
          UPDATE workflow_nodes
          SET status = ?, result = ?, updated_at = datetime('now')
          WHERE workflow_id = ? AND node_id = ?
        ", params = list(status, result_blob, workflow_id, node_id))
      } else {
        DBI::dbExecute(con, "
          UPDATE workflow_nodes
          SET status = ?, updated_at = datetime('now')
          WHERE workflow_id = ? AND node_id = ?
        ", params = list(status, workflow_id, node_id))
      }
    },

    #' @description
    #' Get workflow state for resuming.
    #' @param workflow_id Workflow identifier.
    #' @return A list with workflow nodes and their states.
    get_workflow = function(workflow_id) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      nodes <- DBI::dbGetQuery(con, "
        SELECT * FROM workflow_nodes
        WHERE workflow_id = ?
        ORDER BY id
      ", params = list(workflow_id))

      if (nrow(nodes) == 0) {
        return(NULL)
      }

      # Deserialize results
      nodes$result <- lapply(nodes$result, function(r) {
        if (!is.null(r) && length(r) > 0) {
          tryCatch(unserialize(r), error = function(e) NULL)
        } else {
          NULL
        }
      })

      # Parse dependencies
      nodes$dependencies <- lapply(nodes$dependencies, function(d) {
        if (!is.null(d) && nchar(d) > 0) {
          strsplit(d, ",")[[1]]
        } else {
          character(0)
        }
      })

      list(
        workflow_id = workflow_id,
        nodes = nodes,
        completed = sum(nodes$status == "completed"),
        failed = sum(nodes$status == "failed"),
        pending = sum(nodes$status == "pending")
      )
    },

    #' @description
    #' Resume a failed workflow from the last successful point.
    #' @param workflow_id Workflow identifier.
    #' @return List of node IDs that need to be re-executed.
    get_resumable_nodes = function(workflow_id) {
      workflow <- self$get_workflow(workflow_id)
      if (is.null(workflow)) {
        return(character(0))
      }

      # Find failed and pending nodes
      nodes <- workflow$nodes
      failed_or_pending <- nodes$node_id[nodes$status %in% c("failed", "pending")]

      # Also include nodes that depend on failed nodes
      all_to_run <- failed_or_pending
      for (node_id in failed_or_pending) {
        dependents <- nodes$node_id[sapply(nodes$dependencies, function(d) node_id %in% d)]
        all_to_run <- unique(c(all_to_run, dependents))
      }

      all_to_run
    },

    #' @description
    #' Store a conversation turn for context.
    #' @param session_id Session identifier.
    #' @param role Message role ("user", "assistant", "system").
    #' @param content Message content.
    #' @param metadata Optional metadata list.
    store_conversation = function(session_id, role, content, metadata = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbExecute(con, "
        INSERT INTO conversations (session_id, role, content, metadata, created_at)
        VALUES (?, ?, ?, ?, datetime('now'))
      ", params = list(
        session_id,
        role,
        content,
        if (!is.null(metadata)) jsonlite::toJSON(metadata, auto_unbox = TRUE) else NULL
      ))
    },

    #' @description
    #' Get conversation history for a session.
    #' @param session_id Session identifier.
    #' @param limit Maximum number of messages.
    #' @return A data frame of conversation messages.
    get_conversation = function(session_id, limit = 100) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbGetQuery(con, "
        SELECT role, content, metadata, created_at
        FROM conversations
        WHERE session_id = ?
        ORDER BY created_at DESC
        LIMIT ?
      ", params = list(session_id, limit))
    },

    #' @description
    #' Store or update a human review for an AI-generated chunk.
    #' @param chunk_id Unique identifier for the chunk.
    #' @param file_path Path to the source file.
    #' @param chunk_label Chunk label from knitr.
    #' @param prompt The prompt sent to the AI.
    #' @param response The AI's response.
    #' @param status Review status ("pending", "approved", "rejected").
    #' @param ai_agent Optional agent name.
    #' @param uncertainty Optional uncertainty level.
    #' @param session_id Optional session identifier for transcript/provenance.
    #' @param review_mode Optional normalized review mode.
    #' @param runtime_mode Optional normalized runtime mode.
    #' @param artifact_json Optional JSON review artifact payload.
    #' @param execution_status Optional execution state.
    #' @param execution_output Optional execution output text.
    #' @param final_code Optional finalized executable code.
    #' @param error_message Optional execution or generation error.
    #' @return The database row ID.
    store_review = function(chunk_id, file_path, chunk_label, prompt, response,
                           status = "pending", ai_agent = NULL, uncertainty = NULL,
                           session_id = NULL, review_mode = NULL, runtime_mode = NULL,
                           artifact_json = NULL, execution_status = NULL,
                           execution_output = NULL, final_code = NULL,
                           error_message = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      # Check if exists to preserve created_at and extended fields
      existing <- DBI::dbGetQuery(con, "SELECT * FROM human_reviews WHERE chunk_id = ?",
                                  params = list(chunk_id))
      created_at <- if (nrow(existing) > 0) existing$created_at[1] else now
      reviewed_at <- if (nrow(existing) > 0) existing$reviewed_at[1] else NA_character_

      # Convert NULL to NA for SQL
      ai_agent <- private$coalesce_db_value(ai_agent, existing, "ai_agent")
      uncertainty <- private$coalesce_db_value(uncertainty, existing, "uncertainty")
      session_id <- private$coalesce_db_value(session_id, existing, "session_id")
      review_mode <- private$coalesce_db_value(review_mode, existing, "review_mode")
      runtime_mode <- private$coalesce_db_value(runtime_mode, existing, "runtime_mode")
      artifact_json <- private$coalesce_db_value(artifact_json, existing, "artifact_json")
      execution_status <- private$coalesce_db_value(execution_status, existing, "execution_status")
      execution_output <- private$coalesce_db_value(execution_output, existing, "execution_output")
      final_code <- private$coalesce_db_value(final_code, existing, "final_code")
      error_message <- private$coalesce_db_value(error_message, existing, "error_message")

      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO human_reviews
        (chunk_id, file_path, chunk_label, prompt, response, status, ai_agent, uncertainty,
         session_id, review_mode, runtime_mode, artifact_json, execution_status, execution_output,
         final_code, error_message, reviewed_at, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(chunk_id, file_path, chunk_label, prompt, response, status,
                       ai_agent, uncertainty, session_id, review_mode, runtime_mode, artifact_json,
                       execution_status, execution_output, final_code, error_message, reviewed_at,
                       created_at, now))

      DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    },

    #' @description
    #' Store structured review artifact metadata for a chunk.
    #' @param chunk_id Chunk identifier.
    #' @param session_id Optional session identifier.
    #' @param review_mode Optional normalized review mode.
    #' @param runtime_mode Optional normalized runtime mode.
    #' @param artifact A serializable list representing the review artifact.
    #' @return Invisible TRUE.
    store_review_artifact = function(chunk_id, artifact, session_id = NULL,
                                     review_mode = NULL, runtime_mode = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      existing <- DBI::dbGetQuery(con, "SELECT chunk_id FROM human_reviews WHERE chunk_id = ?",
                                  params = list(chunk_id))
      if (nrow(existing) == 0) {
        rlang::abort(sprintf("Cannot store review artifact: chunk_id '%s' was not found.", chunk_id))
      }

      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      DBI::dbExecute(con, "
        UPDATE human_reviews
        SET session_id = COALESCE(?, session_id),
            review_mode = COALESCE(?, review_mode),
            runtime_mode = COALESCE(?, runtime_mode),
            artifact_json = ?,
            updated_at = ?
        WHERE chunk_id = ?
      ", params = list(
        if (is.null(session_id)) NA_character_ else session_id,
        if (is.null(review_mode)) NA_character_ else review_mode,
        if (is.null(runtime_mode)) NA_character_ else runtime_mode,
        private$serialize_json(artifact),
        now,
        chunk_id
      ))

      invisible(TRUE)
    },

    #' @description
    #' Get a review by chunk ID.
    #' @param chunk_id Chunk identifier.
    #' @return A list with review details, or NULL if not found.
    get_review = function(chunk_id) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      result <- DBI::dbGetQuery(con, "SELECT * FROM human_reviews WHERE chunk_id = ?",
                                params = list(chunk_id))

      if (nrow(result) == 0) return(NULL)
      as.list(result[1, ])
    },

    #' @description
    #' Get a parsed review artifact by chunk ID.
    #' @param chunk_id Chunk identifier.
    #' @return A list artifact, or NULL if none is stored.
    get_review_artifact = function(chunk_id) {
      review <- self$get_review(chunk_id)
      if (is.null(review) || is.null(review$artifact_json) ||
          is.na(review$artifact_json) || !nzchar(review$artifact_json)) {
        return(NULL)
      }

      private$deserialize_json(review$artifact_json)
    },

    #' @description
    #' Get a review together with its parsed artifact.
    #' @param chunk_id Chunk identifier.
    #' @return A list with `review` and `artifact`, or NULL if not found.
    get_review_runtime_record = function(chunk_id) {
      review <- self$get_review(chunk_id)
      if (is.null(review)) {
        return(NULL)
      }

      list(
        review = review,
        artifact = self$get_review_artifact(chunk_id)
      )
    },

    #' @description
    #' Get all reviews for a given source file.
    #' @param file_path Source document path.
    #' @return A data frame of reviews ordered by updated time.
    get_reviews_for_file = function(file_path) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbGetQuery(con, "
        SELECT * FROM human_reviews
        WHERE file_path = ?
        ORDER BY updated_at DESC, chunk_id ASC
      ", params = list(file_path))
    },

    #' @description
    #' Record a saveback lifecycle event for one or more chunk reviews.
    #' @param chunk_ids Character vector of chunk identifiers.
    #' @param source_path Source document path.
    #' @param html_path Optional rendered HTML path.
    #' @param status Saveback status string.
    #' @param rerendered Whether a rerender occurred.
    #' @param message Optional message.
    #' @return Invisible TRUE.
    record_review_saveback = function(chunk_ids, source_path,
                                      html_path = NULL,
                                      status = "requested",
                                      rerendered = FALSE,
                                      message = NULL) {
      chunk_ids <- unique(stats::na.omit(chunk_ids %||% character(0)))
      if (length(chunk_ids) == 0) {
        return(invisible(TRUE))
      }

      payload <- list(
        source_path = source_path,
        html_path = html_path,
        status = status,
        rerendered = rerendered,
        message = message
      )

      for (chunk_id in chunk_ids) {
        self$append_review_event(
          chunk_id = chunk_id,
          event_type = paste0("saveback:", status),
          payload = payload
        )
      }

      invisible(TRUE)
    },

    #' @description
    #' Update execution result fields for a chunk review.
    #' @param chunk_id Chunk identifier.
    #' @param execution_status Execution state string.
    #' @param execution_output Optional execution output text.
    #' @param final_code Optional finalized executable code.
    #' @param error_message Optional execution error.
    #' @return Invisible TRUE.
    update_execution_result = function(chunk_id, execution_status,
                                       execution_output = NULL,
                                       final_code = NULL,
                                       error_message = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      DBI::dbExecute(con, "
        UPDATE human_reviews
        SET execution_status = ?,
            execution_output = ?,
            final_code = ?,
            error_message = ?,
            updated_at = ?
        WHERE chunk_id = ?
      ", params = list(
        execution_status,
        if (is.null(execution_output)) NA_character_ else execution_output,
        if (is.null(final_code)) NA_character_ else final_code,
        if (is.null(error_message)) NA_character_ else error_message,
        now,
        chunk_id
      ))

      invisible(TRUE)
    },

    #' @description
    #' Append an audit event for a reviewed chunk.
    #' @param chunk_id Chunk identifier.
    #' @param event_type Event type string.
    #' @param payload Optional serializable payload list.
    #' @return The database row ID.
    append_review_event = function(chunk_id, event_type, payload = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbExecute(con, "
        INSERT INTO review_events (chunk_id, event_type, payload, created_at)
        VALUES (?, ?, ?, datetime('now'))
      ", params = list(
        chunk_id,
        event_type,
        if (is.null(payload)) NA_character_ else private$serialize_json(payload)
      ))

      DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    },

    #' @description
    #' Update review status.
    #' @param chunk_id Chunk identifier.
    #' @param status New status ("approved" or "rejected").
    update_review_status = function(chunk_id, status) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      DBI::dbExecute(con, "
        UPDATE human_reviews
        SET status = ?, reviewed_at = ?, updated_at = ?
        WHERE chunk_id = ?
      ", params = list(status, now, now, chunk_id))

      self$append_review_event(chunk_id, paste0("status:", status))
    },

    #' @description
    #' Get pending reviews, optionally filtered by file.
    #' @param file_path Optional file path filter.
    #' @return A data frame of pending reviews.
    get_pending_reviews = function(file_path = NULL) {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      if (!is.null(file_path)) {
        DBI::dbGetQuery(con, "
          SELECT * FROM human_reviews
          WHERE status = 'pending' AND file_path = ?
          ORDER BY created_at DESC
        ", params = list(file_path))
      } else {
        DBI::dbGetQuery(con, "
          SELECT * FROM human_reviews
          WHERE status = 'pending'
          ORDER BY created_at DESC
        ")
      }
    },

    #' @description
    #' Get memory statistics.
    #' @return A list with counts and sizes.
    stats = function() {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      list(
        snippets = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM snippets")$n,
        fixes = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM fixes")$n,
        workflow_nodes = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM workflow_nodes")$n,
        conversations = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM conversations")$n,
        human_reviews = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM human_reviews")$n,
        review_events = DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM review_events")$n,
        db_size_mb = round(file.info(self$db_path)$size / (1024^2), 2)
      )
    },

    #' @description
    #' Clear all memory (use with caution).
    #' @param confirm Must be TRUE to proceed.
    clear = function(confirm = FALSE) {
      if (!confirm) {
        message("Set confirm = TRUE to clear all memory.")
        return(invisible(self))
      }

      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbExecute(con, "DELETE FROM snippets")
      DBI::dbExecute(con, "DELETE FROM fixes")
      DBI::dbExecute(con, "DELETE FROM workflow_nodes")
      DBI::dbExecute(con, "DELETE FROM conversations")
      DBI::dbExecute(con, "DELETE FROM human_reviews")
      DBI::dbExecute(con, "DELETE FROM review_events")

      message("Memory cleared.")
      invisible(self)
    },

    #' @description
    #' Print method for ProjectMemory.
    print = function() {
      stats <- self$stats()
      cat("<ProjectMemory>\n")
      cat("  Database:", self$db_path, "\n")
      cat("  Snippets:", stats$snippets, "\n")
      cat("  Fixes:", stats$fixes, "\n")
      cat("  Workflow Nodes:", stats$workflow_nodes, "\n")
      cat("  Conversations:", stats$conversations, "\n")
      cat("  Human Reviews:", stats$human_reviews, "\n")
      cat("  Review Events:", stats$review_events, "\n")
      cat("  Size:", stats$db_size_mb, "MB\n")
      invisible(self)
    }
  ),
  private = list(
    init_db = function() {
      con <- private$get_connection()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      # Create tables if they don't exist
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS snippets (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          code TEXT NOT NULL,
          description TEXT,
          tags TEXT,
          context TEXT,
          code_hash TEXT,
          created_at TEXT NOT NULL
        )
      ")

      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS fixes (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          original_code TEXT NOT NULL,
          error_message TEXT NOT NULL,
          error_signature TEXT,
          fixed_code TEXT NOT NULL,
          fix_description TEXT,
          created_at TEXT NOT NULL
        )
      ")

      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS workflow_nodes (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          workflow_id TEXT NOT NULL,
          node_id TEXT NOT NULL,
          node_type TEXT,
          code TEXT,
          status TEXT DEFAULT 'pending',
          result BLOB,
          dependencies TEXT,
          updated_at TEXT NOT NULL,
          UNIQUE(workflow_id, node_id)
        )
      ")

      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS conversations (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          session_id TEXT NOT NULL,
          role TEXT NOT NULL,
          content TEXT NOT NULL,
          metadata TEXT,
          created_at TEXT NOT NULL
        )
      ")

      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS human_reviews (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          chunk_id TEXT NOT NULL UNIQUE,
          file_path TEXT NOT NULL,
          chunk_label TEXT,
          prompt TEXT NOT NULL,
          response TEXT NOT NULL,
          status TEXT DEFAULT 'pending',
          ai_agent TEXT,
          uncertainty TEXT,
          session_id TEXT,
          review_mode TEXT,
          runtime_mode TEXT,
          artifact_json TEXT,
          execution_status TEXT,
          execution_output TEXT,
          final_code TEXT,
          error_message TEXT,
          reviewed_at TEXT,
          created_at TEXT NOT NULL,
          updated_at TEXT NOT NULL
        )
      ")

      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS review_events (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          chunk_id TEXT NOT NULL,
          event_type TEXT NOT NULL,
          payload TEXT,
          created_at TEXT NOT NULL
        )
      ")

      private$ensure_columns(con, "human_reviews", c(
        session_id = "TEXT",
        review_mode = "TEXT",
        runtime_mode = "TEXT",
        artifact_json = "TEXT",
        execution_status = "TEXT",
        execution_output = "TEXT",
        final_code = "TEXT",
        error_message = "TEXT"
      ))

      # Create indices for faster lookups
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_snippets_hash ON snippets(code_hash)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_fixes_signature ON fixes(error_signature)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_workflow_id ON workflow_nodes(workflow_id)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_session_id ON conversations(session_id)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_chunk_id ON human_reviews(chunk_id)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_review_status ON human_reviews(status)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_review_file ON human_reviews(file_path)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_review_event_chunk ON review_events(chunk_id)")
    },
    get_connection = function() {
      if (!requireNamespace("DBI", quietly = TRUE)) {
        rlang::abort("DBI package required for ProjectMemory. Install with: install.packages('DBI')")
      }
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        rlang::abort("RSQLite package required for ProjectMemory. Install with: install.packages('RSQLite')")
      }

      DBI::dbConnect(RSQLite::SQLite(), self$db_path)
    },
    generate_error_signature = function(error) {
      # Extract key parts of error for matching
      # Remove variable names, line numbers, etc.
      signature <- error

      # Remove file paths
      signature <- gsub("'[^']*\\.(R|r)'", "'<file>'", signature)

      # Remove line numbers
      signature <- gsub("line \\d+", "line <n>", signature)

      # Remove object names in quotes
      signature <- gsub("'[^']*'", "'<obj>'", signature)

      # Remove numbers
      signature <- gsub("\\d+", "<n>", signature)

      # Create hash
      digest::digest(signature, algo = "md5")
    },
    ensure_columns = function(con, table_name, columns) {
      existing_fields <- DBI::dbListFields(con, table_name)
      for (column_name in names(columns)) {
        if (!column_name %in% existing_fields) {
          DBI::dbExecute(
            con,
            sprintf("ALTER TABLE %s ADD COLUMN %s %s", table_name, column_name, columns[[column_name]])
          )
        }
      }
    },
    coalesce_db_value = function(value, existing, field) {
      if (!is.null(value)) {
        return(value)
      }

      if (nrow(existing) > 0 && field %in% names(existing)) {
        existing_value <- existing[[field]][1]
        if (!is.null(existing_value) && !is.na(existing_value)) {
          return(existing_value)
        }
      }

      NA_character_
    },
    serialize_json = function(value) {
      jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
    },
    deserialize_json = function(value) {
      jsonlite::fromJSON(value, simplifyVector = FALSE)
    }
  )
)

#' @title Create Project Memory
#' @description
#' Factory function to create or connect to a project memory database.
#' @param project_root Project root directory.
#' @param db_name Database filename.
#' @return A ProjectMemory object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Create memory for current project
#' memory <- project_memory()
#'
#' # Store a successful code snippet
#' memory$store_snippet(
#'   code = "df %>% filter(x > 0) %>% summarize(mean = mean(y))",
#'   description = "Filter and summarize data",
#'   tags = c("dplyr", "summarize")
#' )
#'
#' # Store an error fix
#' memory$store_fix(
#'   original_code = "mean(df$x)",
#'   error = "argument is not numeric or logical",
#'   fixed_code = "mean(as.numeric(df$x), na.rm = TRUE)",
#'   fix_description = "Convert to numeric and handle NAs"
#' )
#'
#' # Search for relevant snippets
#' memory$search_snippets("summarize")
#' }
#' }
project_memory <- function(project_root = if (interactive()) getwd() else tempdir(), db_name = "memory.sqlite") {
  ProjectMemory$new(project_root, db_name)
}

#' @title Get or Create Global Memory
#' @description
#' Get the global project memory instance, creating it if necessary.
#' @return A ProjectMemory object.
#' @export
get_memory <- function() {
  if (is.null(.aisdk_env$memory)) {
    .aisdk_env$memory <- project_memory()
  }
  .aisdk_env$memory
}

# Package environment for storing global memory instance
.aisdk_env <- new.env(parent = emptyenv())
