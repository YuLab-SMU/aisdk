test_that("Caching works", {
  skip_if_not_installed("memoise")
  
  count <- 0
  my_tool <- tool("counter", "Counts calls", z_object(x = z_integer()), function(args) {
    count <<- count + 1
    paste("Count:", count)
  })
  
  # Wrap with cache
  cached_tool <- cache_tool(my_tool)
  
  # First call
  res1 <- cached_tool$run(list(x = 1))
  expect_equal(res1, "Count: 1")
  expect_equal(count, 1)
  
  # Second call (same args) - should hit cache and NOT increment count
  res2 <- cached_tool$run(list(x = 1))
  expect_equal(res2, "Count: 1")
  expect_equal(count, 1) # Count should still be 1
  
  # Third call (diff args) - should run
  res3 <- cached_tool$run(list(x = 2))
  expect_equal(res3, "Count: 2")
  expect_equal(count, 2)
})
