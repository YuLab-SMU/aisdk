# Create Alibaba Cloud Bailian Provider

Factory function to create an Alibaba Cloud Bailian (百炼) provider
using the DashScope API.

## Usage

``` r
create_bailian(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  DashScope API key. Defaults to DASHSCOPE_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to
  https://dashscope.aliyuncs.com/compatible-mode/v1.

- headers:

  Optional additional headers.

## Value

A BailianProvider object.

## Supported Models

DashScope platform hosts Qwen series and other models:

- **qwen-plus**: Balanced performance model

- **qwen-turbo**: Fast & cost-effective model

- **qwen-max**: Most capable model

- **qwq-32b**: Reasoning model with chain-of-thought

- **qwen-vl-plus**: Vision-language model

- Other third-party models available on the platform

## Examples

``` r
# \donttest{
if (interactive()) {
bailian <- create_bailian()

# Standard chat model
model <- bailian$language_model("qwen-plus")
result <- generate_text(model, "你好")

# Reasoning model (QwQ with chain-of-thought)
model <- bailian$language_model("qwq-32b")
result <- generate_text(model, "Solve: What is 15 * 23?")
print(result$reasoning) # Chain-of-thought reasoning

# Default model (qwen-plus)
model <- bailian$language_model()
}
# }
```
