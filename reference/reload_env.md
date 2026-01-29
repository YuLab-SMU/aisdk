# Reload project-level environment variables

Forces R to re-read the .Renviron file without restarting the session.
This is useful when you've modified .Renviron and don't want to restart
R.

## Usage

``` r
reload_env(path = ".Renviron")
```

## Arguments

- path:

  Path to .Renviron file (default: project root)

## Value

Invisible TRUE if successful

## Examples

``` r
if (FALSE) { # \dontrun{
# Reload environment after modifying .Renviron
reload_env()
# Now use the new keys
Sys.getenv("OPENAI_API_KEY")
} # }
```
