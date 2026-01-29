# Setup file for httptest2 mocking
# httptest2 is optional - only loaded if available

if (requireNamespace("httptest2", quietly = TRUE)) {
  library(httptest2)
  
  # Use recorded responses during testing
  # To record new fixtures, run tests with:
  #   with_mock_api(test_that(...)
  
  # Set redactor for all tests (to hide API keys in recordings)
  set_redactor(function(response) {
    # Redact API keys from recorded responses
    response <- gsub_response(response, "sk-[a-zA-Z0-9]+", "sk-REDACTED")
    response <- gsub_response(response, "sk-ant-[a-zA-Z0-9-]+", "sk-ant-REDACTED")
    response
  })
}
