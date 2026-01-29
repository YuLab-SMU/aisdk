# Telemetry Class

R6 class for logging events in a structured format (JSON).

## Public fields

- `trace_id`:

  Current trace ID for the session.

- `pricing_table`:

  Pricing for common models (USD per 1M tokens).

## Methods

### Public methods

- [`Telemetry$new()`](#method-Telemetry-new)

- [`Telemetry$log_event()`](#method-Telemetry-log_event)

- [`Telemetry$as_hooks()`](#method-Telemetry-as_hooks)

- [`Telemetry$calculate_cost()`](#method-Telemetry-calculate_cost)

- [`Telemetry$clone()`](#method-Telemetry-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize Telemetry

#### Usage

    Telemetry$new(trace_id = NULL)

#### Arguments

- `trace_id`:

  Optional trace ID. If NULL, generates a random one.

------------------------------------------------------------------------

### Method `log_event()`

Log an event

#### Usage

    Telemetry$log_event(type, ...)

#### Arguments

- `type`:

  Event type (e.g., "generation_start", "tool_call").

- `...`:

  Additional fields to log.

------------------------------------------------------------------------

### Method `as_hooks()`

Create hooks for telemetry

#### Usage

    Telemetry$as_hooks()

#### Returns

A HookHandler object pre-configured with telemetry logs.

------------------------------------------------------------------------

### Method `calculate_cost()`

Calculate estimated cost for a generation result

#### Usage

    Telemetry$calculate_cost(result, model_id = NULL)

#### Arguments

- `result`:

  The GenerateResult object.

- `model_id`:

  Optional model ID string. if NULL, tries to guess from context (not
  reliable yet, passing in log_event might be better).

#### Returns

Estimated cost in USD, or NULL if unknown.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Telemetry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
