#' @title Caching System
#' @description
#' Utilities for caching tool execution results and other expensive operations.
#' @name cache
NULL

#' @title Cache Tool
#' @description
#' Wrap a tool with caching capabilities using the `memoise` package.
#' @param tool The Tool object to cache.
#' @param cache An optional memoise cache configuration (e.g., cache_memory() or cache_filesystem()).
#'   Defaults to `memoise::cache_memory()`.
#' @return A new Tool object that caches its execution.
#' @export
cache_tool <- function(tool, cache = NULL) {
  if (is.null(cache)) {
    cache <- memoise::cache_memory()
  }
  
  # create a memoised version of the execute function
  # We need to access the private .execute, so we need a workaround or ensure Tool opens it up.
  # For now, we will wrap the public run method.
  
  memoised_run <- memoise::memoise(function(args) {
    # We call the original tool's run method
    # Note: 'tool' is captured in the closure
    tool$run(args)
  }, cache = cache)
  
  # Create a new tool that delegates to the memoised function
  Tool$new(
    name = tool$name,
    description = paste(tool$description, "(Cached)"),
    parameters = tool$parameters,
    execute = function(args) {
      memoised_run(args)
    }
  )
}
