# Skill Creation Examples

## Example 1: Data Wrangler Skill

```r
# 1. Initialize
aisdk::init_skill(name = "data_wrangler", path = "inst/skills")

# 2. Add Script (scripts/filter_data.R)
# Expects args$df and args$condition
df <- get(args$df, envir = .GlobalEnv)
result <- dplyr::filter(df, eval(parse(text = args$condition)))
assign(paste0(args$df, '_filtered'), result, envir = .GlobalEnv)
return(paste('Filtered data saved to', paste0(args$df, '_filtered')))

# 3. Add Reference (references/cheatsheet.md)
# Add dplyr cheat sheet content...
```
