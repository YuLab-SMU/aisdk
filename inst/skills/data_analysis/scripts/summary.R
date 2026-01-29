
# Note: args is injected by the executor
# args <- list(df_name = "mtcars") # Uncomment for manual testing

if (is.null(args$df_name)) stop("df_name is required")
if (!exists(args$df_name)) stop(paste("Object not found:", args$df_name))

df <- get(args$df_name)
paste(capture.output(summary(df)), collapse = "
")

