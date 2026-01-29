name <- args$name
if (is.null(name) || length(name) == 0 || is.na(name) || !nzchar(name)) {
  name <- "World"
}
sprintf("Hello, %s!", name)
