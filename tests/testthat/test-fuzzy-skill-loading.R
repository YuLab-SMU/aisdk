
# Test fuzzy matching for skill loading

test_that("fuzzy matching works", {
  # Mock registry
  mock_registry <- list(
    list_skills = function() {
      data.frame(
        name = c("stock_analyzer", "data_analysis", "web_scraper"),
        description = c("Analyzes stocks", "Analyzes data", "Scrapes web"),
        stringsAsFactors = FALSE
      )
    },
    get_skill = function(name) {
      if (name %in% c("stock_analyzer", "data_analysis", "web_scraper")) {
        list(
          name = name,
          load = function() "Instructions",
          list_scripts = function() character(0),
          list_resources = function() character(0),
          read_resource = function(name) "Content",
          get_asset_path = function(name) file.path(tempdir(), name),
          path = tempdir()
        )
      } else {
        NULL
      }
    }
  )

  tools <- create_skill_tools(mock_registry)
  load_skill_tool <- tools[[1]] # load_skill is the first tool
  
  # 1. Exact match
  result <- load_skill_tool$run(list(skill_name = "stock_analyzer"))
  expect_match(result, "Instructions")
  
  # 2. Typo: stock_analysis
  result <- load_skill_tool$run(list(skill_name = "stock_analysis"))
  expect_match(result, "Did you mean: stock_analyzer?")
  
  # 3. Typo: web_scrapper
  result <- load_skill_tool$run(list(skill_name = "web_scrapper"))
  expect_match(result, "Did you mean: web_scraper?")
  
  # 4. Unknown skill (no close match)
  result <- load_skill_tool$run(list(skill_name = "completely_random_string_xyz"))
  expect_match(result, "Skill not found: completely_random_string_xyz")
  expect_false(grepl("Did you mean", result))
})
