# aisdk: The AI SDK for R

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/YuLab-SMU/aisdk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YuLab-SMU/aisdk/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![pkgdown](https://github.com/YuLab-SMU/aisdk/actions/workflows/pkgdown.yaml/badge.svg)](https://YuLab-SMU.github.io/aisdk/)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/YuLab-SMU/aisdk)
<!-- badges: end -->

**aisdk** is a production-grade framework for building AI-powered applications in R. It provides a unified interface for multiple model providers (OpenAI, Anthropic), a powerful agentic system, and seamless integration with the R ecosystem (Shiny, RMarkdown, Quarto).

## Features

-   **Unified API**: Switch between OpenAI and Anthropic models with a single line of code.
-   **Agentic Framework**: Built-in support for **Agents**, **Tasks**, and **Flows**.
    -   `CoderAgent`: Writes and edits code.
    -   `PlannerAgent`: Breaks down complex problems.
-   **Tool System**: Turn any R function into an AI-callable tool with automatic schema generation.
-   **Structured Outputs**: Generate type-safe JSON, data frames, and complex objects.
-   **Chat Sessions**: Stateful conversation management with history tracking.
-   **Enterprise Ops**: Telemetry, hooks, cost tracking, and MCP (Model Context Protocol) support.

## Installation

You can install the development version of aisdk from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YuLab-SMU/aisdk")
```

## Quick Start

### Basic Text Generation

``` r
library(aisdk)

file.edit(".env")
## set OPENAI_API_KEY\OPENAI_BASE_URL\OPENAI_MODEL to .env file.

library(dotenv)
load_dot_env()

# Create a model provider and model
provider <- create_openai()
model <- provider$language_model(OPENAI_MODEL)

# Generate text
response <- stream_text(model, "Explain the concept of 'tidy evaluation' in R.")
render_text(response)
```

### Building an Agent

Create an agent with tools to solve tasks:

``` r
# Define a calculator tool
calc_tool <- tool(
  name = "add_numbers",
  description = "Adds two numbers together",
  parameters = z_object(
    a = z_number("First number"),
    b = z_number("Second number")
  ),
  execute = function(args) {
    args$a + args$b
  }
)

# Create an agent
agent <- create_agent(
  name = "Mathematician",
  description = "Solve math problems accurately",
  system_prompt = "You are a helpful mathematician.",
  tools = list(calc_tool)
)

# Run the agent
result <- agent$run("What is 1234 + 5678?", model = model)

# Or stream the response
result <- agent$stream("What is 1234 + 5678?", model = model)

render_text(result)
```

## Documentation

Full documentation is available at [https://YuLab-SMU.github.io/aisdk/](https://YuLab-SMU.github.io/aisdk/).
