# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**aisdk** is a production-grade AI SDK for R that provides a unified interface for building AI-powered applications. It features a layered architecture (Specification, Utilities, Providers, Core), multi-agent orchestration, progressive knowledge loading through skills, and MCP (Model Context Protocol) support.

## Development Commands

### Building and Testing

```r
# Full build pipeline (document, install, check)
Rscript Rbuild.R

# Document only (generate Rd files, NAMESPACE)
devtools::document(roclets = c("rd", "collate", "namespace"))

# Install package locally
devtools::install(upgrade = "never", quick = TRUE)

# Run R CMD check (CRAN standards)
devtools::check(
  document = FALSE,
  cran = TRUE,
  args = c("--no-manual", "--no-vignettes", "--no-build-vignettes")
)

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-agent.R")

# Run tests matching pattern
testthat::test_file("tests/testthat/test-agent.R", filter = "agent_creation")
```

### Environment Setup

```r
# Set API keys in .Renviron (recommended)
file.edit("~/.Renviron")
# Add: OPENAI_API_KEY=sk-...
# Add: ANTHROPIC_API_KEY=sk-ant-...

# Or use .env file with dotenv package
library(dotenv)
load_dot_env()
```

### Package Documentation

```r
# Build pkgdown site
pkgdown::build_site()

# Preview vignettes
devtools::build_vignettes()
```

## Architecture Overview

### Layered Architecture

The codebase follows a strict layered architecture with clear separation of concerns:

1. **Specification Layer** (`spec_model.R`)
   - Defines abstract interfaces: `LanguageModelV1`, `EmbeddingModelV1`, `GenerateResult`
   - All providers must implement these contracts
   - Uses `do_` prefix for internal methods to prevent direct usage

2. **Provider Layer** (`provider_*.R`)
   - Concrete implementations for AI providers (OpenAI, Anthropic, NVIDIA)
   - Each provider translates between aisdk's unified format and provider-specific APIs
   - Key differences handled: system prompts, tool calls, message formats, streaming

3. **Utilities Layer** (`utils_*.R`)
   - Cross-cutting concerns: HTTP, JSON parsing, middleware, caching, telemetry
   - `utils_json.R`: JSON repair for malformed LLM outputs
   - `utils_http.R`: Retry logic with exponential backoff
   - `utils_mcp.R`: MCP protocol serialization

4. **Core API Layer** (`core_api.R`)
   - User-facing functions: `generate_text()`, `stream_text()`, `create_embeddings()`
   - Implements ReAct loop for automatic tool execution
   - Orchestrates lower layers with hooks and middleware

### Agent System

**Key Principle**: Agents are stateless workers that hold capabilities (tools, persona) but not conversation state.

- **Agent** (`agent.R`): Stateless execution unit with name, description, system prompt, tools, and skills
  - `run()`: Execute task with ReAct loop (non-streaming)
  - `stream()`: Execute with streaming output
  - `as_tool()`: Convert agent to Tool for delegation
  - `create_session()`: Create stateful ChatSession

- **Tool** (`tool.R`): Bridge between LLM function calls and R functions
  - Multi-layer defense: tool call repair, invalid tool routing, argument parsing, error capture
  - Auto-infers JSON Schema from function signatures
  - Supports both `execute(args)` and `execute(a, b, c)` styles
  - **Tool Layer Attribute**: Tools can be marked as "llm" (loaded into context) or "computer" (executed via bash/filesystem)

- **Computer Abstraction** (`computer.R`): **NEW - 2026 Agent Design Pattern**
  - Implements hierarchical action space following Manus/Claude Code architecture
  - Provides atomic tools: `bash`, `read_file`, `write_file`, `execute_r_code`
  - Reduces context window usage by 30-50% by pushing actions to computer layer
  - Sandbox modes: "strict", "permissive", "none"
  - Execution logging for observability
  - Usage: `create_computer_tools(working_dir, sandbox_mode)`
  - Multi-layer defense: tool call repair, invalid tool routing, argument parsing, error capture
  - Auto-infers JSON Schema from function signatures
  - Supports both `execute(args)` and `execute(a, b, c)` styles

- **Session** (`session.R`): Stateful conversation management
  - Tracks history, shared memory, shared environment, token usage
  - Multi-agent support via shared state
  - Methods: `send()`, `send_stream()`, `append_message()`, `get_history()`

- **Team** (`team.R`): Multi-agent orchestration using Manager-Worker pattern
  - Dynamically synthesizes Manager agent with `delegate_task` tool
  - Manager routes tasks to appropriate workers based on capabilities
  - Shared session enables complex multi-step workflows

### Skills System

Skills provide progressive knowledge loading - agents load specialized knowledge only when needed, saving context window space.

**Structure**:
```
inst/skills/
  skill_name/
    SKILL.md          # Metadata (name, description) + instructions
    scripts/          # Executable R scripts
    references/       # Documentation, examples, knowledge base
    assets/           # Data files, images, etc.
```

**Key Files**:
- `skill.R`: Skill class and loading logic
- `skill_registry.R`: Discovery and management
- `scaffold.R`: `init_skill()` and `package_skill()` helpers

**Usage**:
```r
# Initialize new skill
init_skill("my_skill", path = "inst/skills")

# Package skill for distribution
package_skill("inst/skills/my_skill", output_dir = ".")

# Load skill in agent
registry <- create_skill_registry(system.file("skills", package = "aisdk"))
skill_tools <- create_skill_tools(registry)
agent <- create_agent(tools = skill_tools, ...)
```

### MCP (Model Context Protocol)

**Server** (`mcp_server.R`): Expose R functions as MCP tools
- Stdio transport for local processes
- SSE transport for remote/web clients
- Automatic schema generation from R functions

**Client** (`mcp_client.R`): Connect to MCP servers
- Discover and use remote tools
- Integrate external capabilities into agents

## Code Conventions

### R6 Classes

All major components use R6 for object-oriented design:
- `lock_objects = FALSE` for dynamic field addition (e.g., `GenerateResult`)
- Private methods prefixed with `.` (e.g., `.validate()`)
- Public API methods documented with roxygen2

### Error Handling

- Use `rlang::abort()` for errors with structured conditions
- Implement retry logic with exponential backoff for API calls
- Graceful degradation: invalid tool calls route to `__invalid__` handler

### Testing

- Tests use `testthat` (edition 3)
- Mock API responses with `httptest2`
- Helper files: `helper-env.R` (environment setup), `helper-mock.R` (mock objects)
- Test naming: `test-<component>.R` (e.g., `test-agent.R`)

### Documentation

- All exported functions must have roxygen2 documentation
- Use `@title`, `@description`, `@param`, `@return`, `@export`
- Examples should use `\dontrun{}` if they require API keys
- Vignettes in `vignettes/` use RMarkdown with `eval = FALSE` for API calls

## Important Patterns

### ReAct Loop

The core execution pattern for agents with tools:
```
while (step < max_steps):
  1. Call model with current messages
  2. If tool_calls present:
     - Execute tools (with hooks and session environment)
     - Append tool results to history
     - Continue loop
  3. Else: break (final answer received)
```

Implemented in `core_api.R:generate_text()` and used by agents.

### Provider Abstraction

When adding a new provider:
1. Create `provider_<name>.R`
2. Implement `LanguageModelV1` interface
3. Handle provider-specific message formats in `get_history_format()`
4. Handle provider-specific tool result formats in `format_tool_result()`
5. Register with `create_<name>()` factory function

### Skill Creation

Skills follow the "textbook" structure:
1. `SKILL.md` with YAML frontmatter (name, description) + instructions
2. `scripts/` for executable R code
3. `references/` for documentation and knowledge
4. `assets/` for data files

Agents load skills dynamically using `load_skill` and `execute_skill_script` tools.

## Key Files Reference

- `R/core_api.R`: Main user-facing API (`generate_text`, `stream_text`)
- `R/agent.R`: Agent class and execution logic
- `R/tool.R`: Tool system with multi-layer defense + tool layer attribute
- `R/computer.R`: Computer abstraction layer (atomic tools, hierarchical action space)
- `R/session.R`: Stateful conversation management
- `R/skill.R`: Skill loading and execution
- `R/mcp_server.R`, `R/mcp_client.R`: MCP protocol implementation
- `R/provider_anthropic.R`, `R/provider_openai.R`: Provider implementations

## Testing Notes

- Tests require API keys in environment variables (or use mocks)
- Use `skip_if_no_api_key()` helper for integration tests
- Mock objects in `helper-mock.R` for unit tests
- Snapshot tests for complex outputs (use `testthat::expect_snapshot()`)

### Tests must be hermetic — do not trust the local environment

These bite repeatedly in this repo; check for them before pushing:

- **Isolate from `.Renviron`.** Anything that reads `Sys.getenv("FOO", default)`
  in package code must be wrapped with `withr::with_envvar(c(FOO = NA), ...)`
  in the test that asserts the "default" branch. Reason: `devtools::test()`
  reuses the current session env, but `devtools::check()` + GitHub Actions
  spin up a fresh R that re-reads `.Renviron`, and the developer's local
  override silently fails the test only there. Real precedent: the deepseek
  default-model test (`test-provider-deepseek.R:29`).
- **Never assert on async OS state with a single check.** If the test
  inspects external state that the kernel/OS reaps asynchronously
  (`pgrep` for child processes, `file.exists()` for unlinked temp files,
  port-listening, etc.), poll with a deadline instead of `Sys.sleep(0.5)`
  + one assertion. macOS reaps in milliseconds; a busy Linux CI runner can
  take seconds. Pattern:
  ```r
  ok <- FALSE
  for (i in seq_len(80)) {
    Sys.sleep(0.1)
    if (<condition>) { ok <- TRUE; break }
  }
  expect_true(ok, info = "...")
  ```
- **Subprocess credentials and env behavior:** `callr::r()` / `callr::r_bg()`'s
  `env` argument is **additive** (child inherits parent env + your additions).
  To actually *remove* a sensitive var from the subprocess, set it to `""`
  AND start the subprocess with `--no-environ` so `.Renviron` cannot put it
  back. Just unsetting it in `Sys.getenv()` accounting is not enough.
  See `R/r_introspect_tools.R::r_eval_build_env()`.

### CI ≠ local check: always check both

A clean `devtools::check()` locally does **not** mean GitHub Actions will pass.
Differences that have caused real failures:

| Difference | Why it matters |
| --- | --- |
| Fresh R subprocess re-reads `.Renviron` | Local overrides invisibly leak in |
| Linux process-reaping latency | Race-y `pgrep`/`ps`-based assertions |
| Different default locale (`LANG`, `LC_*`) | String collation, encoding tests |
| `tempdir()` on Linux is `/tmp` (shorter) | Path-length tests behave differently |
| GA runners have no network for many providers | More `SKIP`s than locally, may surface a latent test that *only* runs in skip-fallback paths |

After pushing a non-trivial PR, run
`gh run watch` or `gh run view --log-failed <run-id>` and treat a red run as
unfinished work, not "flaky CI". Re-running rarely fixes a real race.

## CI/CD

- GitHub Actions: `.github/workflows/R-CMD-check.yaml`
- Runs on Windows and Ubuntu with R release version
- Checks: R CMD check with `--no-manual`, `--as-cran`, `--no-vignettes`
- pkgdown site deployed via `.github/workflows/pkgdown.yaml`
- After `git push`, check the run: `gh run list --branch main --limit 1`.
  If it failed, pull the failing test name with
  `gh run view <id> --log-failed | grep -E "Failure|Error"`. Do not assume
  a CI failure is unrelated to the push without reading the log.
