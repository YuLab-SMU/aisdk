# R-Native Programmatic Sandbox

SandboxManager R6 class and utilities for building an R-native
programmatic tool sandbox. Inspired by Anthropic's Programmatic Tool
Calling, this module enables LLMs to write R code that batch-invokes
registered tools and processes data locally (using dplyr/purrr),
returning only concise results to the context.

## Details

The core idea: instead of the LLM making N separate tool calls (each
requiring a round-trip), it writes a single R script that loops over
inputs, calls tools as ordinary R functions, filters/aggregates the
results with dplyr, and [`print()`](https://rdrr.io/r/base/print.html)s
only the key findings. This dramatically reduces token usage, latency,
and context window pressure.

### Architecture

    User Tools ──► SandboxManager ──► Isolated R Env
                    │                    ├── tool_a()
                    │                    ├── tool_b()
                    │                    ├── dplyr::*
                    │                    └── purrr::*
                    │
                    ▼
                create_r_code_tool() ──► Single "execute_r_code" Tool
                                         (registered with LLM)
