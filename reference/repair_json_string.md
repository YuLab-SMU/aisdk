# Repair JSON String

Attempt to repair common JSON malformations from LLM outputs. This is a
lightweight repair function for common issues. For more complex repairs,
use fix_json() from utils_json.R.

Handles:

- Missing closing braces/brackets

- Trailing commas

- Unquoted keys

- Truncated strings

- Single quotes instead of double quotes

## Usage

``` r
repair_json_string(json_str)
```

## Arguments

- json_str:

  The potentially malformed JSON string.

## Value

A repaired JSON string (best effort).
