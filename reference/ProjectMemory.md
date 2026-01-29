# Project Memory Class

R6 class for managing persistent project memory using SQLite. Stores
code snippets, error fixes, and execution graphs for resuming failed
long-running jobs.

## Public fields

- `db_path`:

  Path to the SQLite database file.

- `project_root`:

  Root directory of the project.

## Methods

### Public methods

- [`ProjectMemory$new()`](#method-ProjectMemory-new)

- [`ProjectMemory$store_snippet()`](#method-ProjectMemory-store_snippet)

- [`ProjectMemory$store_fix()`](#method-ProjectMemory-store_fix)

- [`ProjectMemory$find_similar_fix()`](#method-ProjectMemory-find_similar_fix)

- [`ProjectMemory$search_snippets()`](#method-ProjectMemory-search_snippets)

- [`ProjectMemory$store_workflow_node()`](#method-ProjectMemory-store_workflow_node)

- [`ProjectMemory$update_node_status()`](#method-ProjectMemory-update_node_status)

- [`ProjectMemory$get_workflow()`](#method-ProjectMemory-get_workflow)

- [`ProjectMemory$get_resumable_nodes()`](#method-ProjectMemory-get_resumable_nodes)

- [`ProjectMemory$store_conversation()`](#method-ProjectMemory-store_conversation)

- [`ProjectMemory$get_conversation()`](#method-ProjectMemory-get_conversation)

- [`ProjectMemory$stats()`](#method-ProjectMemory-stats)

- [`ProjectMemory$clear()`](#method-ProjectMemory-clear)

- [`ProjectMemory$print()`](#method-ProjectMemory-print)

- [`ProjectMemory$clone()`](#method-ProjectMemory-clone)

------------------------------------------------------------------------

### Method `new()`

Create or connect to a project memory database.

#### Usage

    ProjectMemory$new(project_root = getwd(), db_name = "memory.sqlite")

#### Arguments

- `project_root`:

  Project root directory. Defaults to current working directory.

- `db_name`:

  Database filename. Defaults to "memory.sqlite".

#### Returns

A new ProjectMemory object.

------------------------------------------------------------------------

### Method `store_snippet()`

Store a successful code snippet for future reference.

#### Usage

    ProjectMemory$store_snippet(
      code,
      description = NULL,
      tags = NULL,
      context = NULL
    )

#### Arguments

- `code`:

  The R code that was executed successfully.

- `description`:

  Optional description of what the code does.

- `tags`:

  Optional character vector of tags for categorization.

- `context`:

  Optional context about when/why this code was used.

#### Returns

The ID of the stored snippet.

------------------------------------------------------------------------

### Method `store_fix()`

Store an error fix for learning.

#### Usage

    ProjectMemory$store_fix(
      original_code,
      error,
      fixed_code,
      fix_description = NULL
    )

#### Arguments

- `original_code`:

  The code that produced the error.

- `error`:

  The error message.

- `fixed_code`:

  The corrected code.

- `fix_description`:

  Description of what was fixed.

#### Returns

The ID of the stored fix.

------------------------------------------------------------------------

### Method `find_similar_fix()`

Find a similar fix from memory.

#### Usage

    ProjectMemory$find_similar_fix(error)

#### Arguments

- `error`:

  The error message to match.

#### Returns

A list with the fix details, or NULL if not found.

------------------------------------------------------------------------

### Method `search_snippets()`

Search for relevant code snippets.

#### Usage

    ProjectMemory$search_snippets(query, limit = 10)

#### Arguments

- `query`:

  Search query (matches description, tags, or code).

- `limit`:

  Maximum number of results.

#### Returns

A data frame of matching snippets.

------------------------------------------------------------------------

### Method `store_workflow_node()`

Store execution graph node for workflow persistence.

#### Usage

    ProjectMemory$store_workflow_node(
      workflow_id,
      node_id,
      node_type,
      code,
      status = "pending",
      result = NULL,
      dependencies = NULL
    )

#### Arguments

- `workflow_id`:

  Unique identifier for the workflow.

- `node_id`:

  Unique identifier for this node.

- `node_type`:

  Type of node (e.g., "transform", "model", "output").

- `code`:

  The code for this node.

- `status`:

  Node status ("pending", "running", "completed", "failed").

- `result`:

  Optional serialized result.

- `dependencies`:

  Character vector of node IDs this depends on.

#### Returns

The database row ID.

------------------------------------------------------------------------

### Method `update_node_status()`

Update workflow node status.

#### Usage

    ProjectMemory$update_node_status(workflow_id, node_id, status, result = NULL)

#### Arguments

- `workflow_id`:

  Workflow identifier.

- `node_id`:

  Node identifier.

- `status`:

  New status.

- `result`:

  Optional result to store.

------------------------------------------------------------------------

### Method `get_workflow()`

Get workflow state for resuming.

#### Usage

    ProjectMemory$get_workflow(workflow_id)

#### Arguments

- `workflow_id`:

  Workflow identifier.

#### Returns

A list with workflow nodes and their states.

------------------------------------------------------------------------

### Method `get_resumable_nodes()`

Resume a failed workflow from the last successful point.

#### Usage

    ProjectMemory$get_resumable_nodes(workflow_id)

#### Arguments

- `workflow_id`:

  Workflow identifier.

#### Returns

List of node IDs that need to be re-executed.

------------------------------------------------------------------------

### Method `store_conversation()`

Store a conversation turn for context.

#### Usage

    ProjectMemory$store_conversation(session_id, role, content, metadata = NULL)

#### Arguments

- `session_id`:

  Session identifier.

- `role`:

  Message role ("user", "assistant", "system").

- `content`:

  Message content.

- `metadata`:

  Optional metadata list.

------------------------------------------------------------------------

### Method `get_conversation()`

Get conversation history for a session.

#### Usage

    ProjectMemory$get_conversation(session_id, limit = 100)

#### Arguments

- `session_id`:

  Session identifier.

- `limit`:

  Maximum number of messages.

#### Returns

A data frame of conversation messages.

------------------------------------------------------------------------

### Method `stats()`

Get memory statistics.

#### Usage

    ProjectMemory$stats()

#### Returns

A list with counts and sizes.

------------------------------------------------------------------------

### Method `clear()`

Clear all memory (use with caution).

#### Usage

    ProjectMemory$clear(confirm = FALSE)

#### Arguments

- `confirm`:

  Must be TRUE to proceed.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for ProjectMemory.

#### Usage

    ProjectMemory$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProjectMemory$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
