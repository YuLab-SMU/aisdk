# Find Closest Match

Find the closest matching string using Levenshtein distance.

## Usage

``` r
find_closest_match(target, candidates, max_distance = 3)
```

## Arguments

- target:

  The target string to match.

- candidates:

  A vector of candidate strings.

- max_distance:

  Maximum allowed edit distance (default 3).

## Value

The closest match, or NULL if none within max_distance.
