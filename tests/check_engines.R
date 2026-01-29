
library(knitr)
cat("Available engines:\n")
print(names(knit_engines$get()))

cat("\nAttempting to get 'r' engine:\n")
eng_r <- knit_engines$get("r")
if (is.null(eng_r)) {
  cat("'r' engine is NULL\n")
} else {
  cat("'r' engine found\n")
}

cat("\nAttempting to get 'R' engine:\n")
eng_R <- knit_engines$get("R")
if (is.null(eng_R)) {
  cat("'R' engine is NULL\n")
} else {
  cat("'R' engine found\n")
}
