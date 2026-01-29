# Create NVIDIA Provider

Factory function to create a NVIDIA provider.

## Usage

``` r
create_nvidia(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  NVIDIA API key. Defaults to NVIDIA_API_KEY env var.

- base_url:

  Base URL. Defaults to "https://integrate.api.nvidia.com/v1".

- headers:

  Optional additional headers.

## Value

A NvidiaProvider object.

## Examples

``` r
if (FALSE) { # \dontrun{
nvidia <- create_nvidia()
model <- nvidia$language_model("z-ai/glm4.7")

# Enable thinking/reasoning
result <- generate_text(model, "Who are you?", 
  chat_template_kwargs = list(enable_thinking = TRUE))
print(result$reasoning)
} # }
```
