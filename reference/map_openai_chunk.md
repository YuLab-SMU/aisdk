# Map OpenAI SSE chunk to aggregator calls

Translates an OpenAI Chat Completions SSE data chunk into the
appropriate SSEAggregator method calls.

## Usage

``` r
map_openai_chunk(data, done, agg)
```

## Arguments

- data:

  Parsed JSON data from SSE event (or NULL if done).

- done:

  Logical, TRUE if stream is complete.

- agg:

  An SSEAggregator instance.
