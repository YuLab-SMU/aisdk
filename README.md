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

-   **🚀 Genesis V2 (NEW!)**: Plan-Execute-Refine architecture with automatic quality assurance
    -   **Genesis V1**: One-line execution with auto-discovery: `genesis("Analyze iris dataset")`
    -   **Genesis V2**: Iterative refinement with quality checks: `genesis_v2("Complex task", auto_refine = TRUE)`
    -   Automatic agent discovery and team assembly
    -   AI-powered team composition via Architect agent
    -   Quality evaluation and automatic improvement
    -   Smart caching for performance
-   **Unified API**: Switch between OpenAI and Anthropic models with a single line of code.
-   **Agentic Framework**: Built-in support for **Agents**, **Tasks**, and **Flows**.
    -   `CoderAgent`: Writes and edits code.
    -   `PlannerAgent`: Breaks down complex problems.
    -   **Multi-Agent Teams**: Coordinate multiple specialized agents
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

### 🚀 Genesis: Zero-Configuration Execution

#### Genesis V1: Simple & Fast

The easiest way to use aisdk - just describe what you want:

``` r
library(aisdk)

# Set your API key
Sys.setenv(ANTHROPIC_API_KEY = "your-api-key")

# One line - that's it!
result <- genesis("Analyze the iris dataset and create a scatter plot")

# With verbose output to see what's happening
result <- genesis(
  "Analyze the mtcars dataset and summarize key statistics",
  verbose = TRUE
)
```

**What Genesis V1 does automatically:**
1. Discovers available agents from your skill library
2. Analyzes your task to understand requirements
3. Selects the optimal combination of agents
4. Assembles a team and executes the task

#### Genesis V2: Plan-Execute-Refine (NEW!)

For complex tasks requiring quality assurance and iterative improvement:

``` r
# Automatic quality checks and refinement
result <- genesis_v2(
  "Analyze iris dataset and create a comprehensive report with visualizations",
  max_iterations = 3,
  quality_threshold = 80,
  auto_refine = TRUE,
  verbose = TRUE
)

# Check results
print_genesis_v2_result(result)

# Analyze execution history
analyze_genesis_history(result)
```

**What Genesis V2 adds:**
1. **Plan**: Architect defines success criteria
2. **Execute**: Team runs the task
3. **Refine**: Evaluator checks quality, Refiner suggests improvements
4. **Iterate**: Automatically retry or replan until quality threshold is met

**Learn more**:
- [Genesis User Guide](docs/GENESIS_USER_GUIDE.md)
- [Genesis V2 PER Design](docs/GENESIS_V2_PER_DESIGN.md)
- [V1 vs V2 Comparison](docs/GENESIS_V1_VS_V2.md)

---

### Basic Text Generation

``` r
library(aisdk)

file.edit(".env")
## set OPENAI_API_KEY\OPENAI_BASE_URL\OPENAI_MODEL to .env file.

library(dotenv)
load_dot_env()

# Create a model provider and model
provider <- create_openai()
model <- provider$language_model(Sys.getenv("OPENAI_MODEL"))

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

### Stateful Conversations

Agents are stateless by default. To have a multi-turn conversation where the agent remembers previous interactions, create a *Chat Session* from the agent:

``` r
# Create a session from the agent
session <- agent$create_session(model = model)

# First interaction
session$send("What is 1234 + 5678?")

# Follow-up (context is preserved)
session$send("Divide that by 2") 
```

### Skills System

The Skills system allows you to package specialized knowledge and tools that can be dynamically loaded by agents. This saves context window space and keeps your agents focused.

**Use the Demo Skill**:

``` r
# Initialize the registry and tools
registry <- create_skill_registry(system.file("skills", package = "aisdk"))
skill_tools <- create_skill_tools(registry)

# Create an agent
analyst <- create_agent(
  name = "DataAnalyst",
  description = "A data analysis agent",
  system_prompt = "You are an expert data analyst. Use the available skills to help the user.",
  tools = skill_tools # <--- Pass the list of tools, NOT the registry
)

# Run the agent
analyst$stream("Load the data_analysis skill and summarize the mtcars dataset.", model = model)
```

**Example Output**:

```
── ❯ Thinking...    
─ 
─  The user wants me to: 
─  1. Load the data_analysis skill 
─  2. Summarize the mtcars dataset 
─   
─  Let me start by loading the skill to see what scripts are available. 
─ 
── ✔ Done thinking (5 lines) 

  (ℹ Hide with options(aisdk.show_thinking = FALSE)) 


I'll help you load the data_analysis skill and summarize the mtcars dataset. Let me start by loading the skill.
ℹ Calling tool load_skill {"skill_name":"data_analysis"}
✔ Tool load_skill returned: "# Instructions Use the `analyze.R` script to analyze datasets.  ## Available Datasets - iris: Fisher's Iris dataset (150 observations, 5
variables) - mtcars: Motor Trend Car Road Tests (32 observat..."── ❯ Thinking...    
─ 
─  Now I can see that the data_analysis skill has an analyze.R script that can analyze datasets. The mtcars dataset is available, and I should use the analyze.R script with the dataset parameter set to "mtcars". I'll use the default top_n value of 5 as shown in the example. 
─ 
── ✔ Done thinking (1 lines) 

  (ℹ Hide with options(aisdk.show_thinking = FALSE)) 


▶ Calling tool execute_skill_script {"skill_name":"data_analysis","script_name":"analyze.R","args":{"dataset":"mtcars","top_n":5}}
✔ Tool execute_skill_script returned: "Dataset: mtcars Dimensions: 32 rows x 11 cols Columns: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb Numeric Summary:
mpg: min=10.40, max=33.90, mean=20.09 cyl: min=4.00, max=8.00, mean=..."── ❯ Thinking...    
─ 
─  Great! I've successfully loaded the data_analysis skill and analyzed the mtcars dataset. The output provides a comprehensive summary of the dataset including: 
─   
─  1. Dataset dimensions (32 rows x 11 columns) 
─  2. Column names 
─  3. Numeric summary statistics (min, max, mean) for all variables 
─  4. First 5 rows of the dataset 
─   
─  This gives a good overview of the mtcars dataset, which contains data about Motor Trend car road tests with various performance and design characteristics for 32 different car models. 
─ 
── ✔ Done thinking (8 lines) 

  (ℹ Hide with options(aisdk.show_thinking = FALSE)) 


I've successfully loaded the data_analysis skill and analyzed the mtcars dataset. Here's a summary:


── Dataset Overview ──- Name: mtcars (Motor Trend Car Road Tests)
- Dimensions: 32 observations × 11 variables
- Variables: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb

── Key Statistics Summary ──
| Variable | Min   | Max    | Mean   | Description                     |
| -------- | ----- | ------ | ------ | ------------------------------- |
| mpg      | 10.40 | 33.90  | 20.09  | Miles per gallon                |
| cyl      | 4.00  | 8.00   | 6.19   | Number of cylinders             |
| disp     | 71.10 | 472.00 | 230.72 | Displacement (cu.in.)           |
| hp       | 52.00 | 335.00 | 146.69 | Gross horsepower                |
| drat     | 2.76  | 4.93   | 3.60   | Rear axle ratio                 |
| wt       | 1.51  | 5.42   | 3.22   | Weight (1000 lbs)               |
| qsec     | 14.50 | 22.90  | 17.85  | 1/4 mile time                   |
| vs       | 0.00  | 1.00   | 0.44   | Engine type (0=V, 1=Straight)   |
| am       | 0.00  | 1.00   | 0.41   | Transmission (0=Auto, 1=Manual) |
| gear     | 3.00  | 5.00   | 3.69   | Number of forward gears         |
| carb     | 1.00  | 8.00   | 2.81   | Number of carburetors           |

── Sample Data (First 5 Rows) ──The dataset includes classic cars like the Mazda RX4 (21.0 mpg), Datsun 710 (22.8 mpg), and Hornet models, showing a range of performance characteristics from fuel economy to horsepower and weight.

This dataset is commonly used for regression analysis and exploring relationships between car design parameters and fuel efficiency.
```

## Documentation

Full documentation is available at [https://YuLab-SMU.github.io/aisdk/](https://YuLab-SMU.github.io/aisdk/).
