# Update .Renviron with new values

Updates or appends environment variables to the .Renviron file.

## Usage

``` r
update_renviron(updates, path = ".Renviron")
```

## Arguments

- updates:

  A named list of key-value pairs to update.

- path:

  Path to .Renviron file (default: project root)

## Value

Invisible TRUE if successful
