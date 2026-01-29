# Add Stable IDs to Nested List

Recursively traverses a list and adds `_id` fields where missing based
on content hashing.

## Usage

``` r
add_stable_ids(x, prefix = NULL)
```

## Arguments

- x:

  List to process.

- prefix:

  Optional prefix for IDs.

## Value

Modified list.
