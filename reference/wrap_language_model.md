# Wrap Language Model with Middleware

Wraps a LanguageModelV1 with one or more middleware instances.
Middleware is applied in order: first middleware transforms first, last
middleware wraps closest to the model.

## Usage

``` r
wrap_language_model(model, middleware, model_id = NULL, provider_id = NULL)
```

## Arguments

- model:

  A LanguageModelV1 object.

- middleware:

  A single Middleware object or a list of Middleware objects.

- model_id:

  Optional custom model ID.

- provider_id:

  Optional custom provider ID.

## Value

A new LanguageModelV1 object with middleware applied.
