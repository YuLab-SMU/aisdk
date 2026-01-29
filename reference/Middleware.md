# Middleware (Base Class)

Defines a middleware that can intercept and modify model operations.

## Public fields

- `name`:

  A descriptive name for this middleware.

## Methods

### Public methods

- [`Middleware$transform_params()`](#method-Middleware-transform_params)

- [`Middleware$wrap_generate()`](#method-Middleware-wrap_generate)

- [`Middleware$wrap_stream()`](#method-Middleware-wrap_stream)

- [`Middleware$clone()`](#method-Middleware-clone)

------------------------------------------------------------------------

### Method `transform_params()`

Transform parameters before calling the model.

#### Usage

    Middleware$transform_params(params, type, model)

#### Arguments

- `params`:

  The original call parameters.

- `type`:

  Either "generate" or "stream".

- `model`:

  The model being called.

#### Returns

The transformed parameters.

------------------------------------------------------------------------

### Method `wrap_generate()`

Wrap the generate operation.

#### Usage

    Middleware$wrap_generate(do_generate, params, model)

#### Arguments

- `do_generate`:

  A function that calls the model's do_generate.

- `params`:

  The (potentially transformed) parameters.

- `model`:

  The model being called.

#### Returns

The result of the generation.

------------------------------------------------------------------------

### Method `wrap_stream()`

Wrap the stream operation.

#### Usage

    Middleware$wrap_stream(do_stream, params, model, callback)

#### Arguments

- `do_stream`:

  A function that calls the model's do_stream.

- `params`:

  The (potentially transformed) parameters.

- `model`:

  The model being called.

- `callback`:

  The streaming callback function.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Middleware$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
