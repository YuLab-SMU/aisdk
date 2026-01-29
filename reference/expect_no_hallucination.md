# Expect No Hallucination

Test that an LLM response does not contain hallucinated information when
compared against ground truth.

## Usage

``` r
expect_no_hallucination(
  response,
  ground_truth,
  model = NULL,
  tolerance = 0.1,
  info = NULL
)
```

## Arguments

- response:

  The LLM response to check.

- ground_truth:

  The factual information to check against.

- model:

  Model to use for checking.

- tolerance:

  Allowed deviation (0 = strict, 1 = lenient).

- info:

  Additional information for failure message.
