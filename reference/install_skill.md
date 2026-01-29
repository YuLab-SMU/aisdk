# Install a Skill

Install a skill from the global skill store or a GitHub repository.

## Usage

``` r
install_skill(skill_ref, version = NULL, force = FALSE)
```

## Arguments

- skill_ref:

  Skill reference (e.g., "username/skillname").

- version:

  Optional specific version.

- force:

  Force reinstallation.

## Value

The installed Skill object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install from GitHub
install_skill("aisdk/data-analysis")

# Install specific version
install_skill("aisdk/visualization", version = "1.2.0")

# Force reinstall
install_skill("aisdk/ml-tools", force = TRUE)
} # }
```
