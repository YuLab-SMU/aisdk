# Create Gemini Provider

Factory function to create a Gemini provider.

## Usage

``` r
create_gemini(api_key = NULL, base_url = NULL, headers = NULL, name = NULL)
```

## Arguments

- api_key:

  Gemini API key. Defaults to GEMINI_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to
  https://generativelanguage.googleapis.com/v1beta/models.

- headers:

  Optional additional headers.

- name:

  Optional provider name override.

## Value

A GeminiProvider object.

## Examples

``` r
# \donttest{
if (interactive()) {
gemini <- create_gemini(api_key = "AIza...")
model <- gemini$language_model("gemini-1.5-pro")
}
# }
```
