# Map native Anthropic SSE event to aggregator calls

Translates a native Anthropic Messages API SSE event into the
appropriate SSEAggregator method calls.

## Usage

``` r
map_anthropic_chunk(event_type, event_data, agg)
```

## Arguments

- event_type:

  SSE event type string (e.g. "content_block_delta").

- event_data:

  Parsed JSON data from SSE event.

- agg:

  An SSEAggregator instance.

## Value

Logical TRUE if the stream should break (message_stop received).
