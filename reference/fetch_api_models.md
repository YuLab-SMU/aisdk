# Fetch available models from API provider

Fetch available models from API provider

## Usage

``` r
fetch_api_models(provider, api_key = NULL, base_url = NULL)
```

## Arguments

- provider:

  Provider name ("openai", "nvidia", "anthropic", etc.)

- api_key:

  API key

- base_url:

  Base URL

## Value

A data frame with 'id' column and capability flag columns
