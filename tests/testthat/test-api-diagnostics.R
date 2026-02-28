test_that("check_api handles url correctly but connection may fail locally", {
    # We suppress messages/warnings to keep CRAN test logs clean
    out <- suppressMessages(suppressWarnings({
        check_api(url = "http://127.0.0.1:0/invalid")
    }))

    expect_type(out, "list")
    expect_true(!is.null(out$has_internet))
    expect_equal(out$host, "127.0.0.1")
})

test_that("check_api handles missing params", {
    expect_error(check_api(), "Could not determine URL")
})

test_that("check_api handles model", {
    MockDiagnosticsModel <- R6::R6Class("MockDiagnosticsModel",
        inherit = MockModel,
        public = list(
            get_config = function() list(base_url = "https://mock-api.com/v1")
        )
    )

    mock_model <- MockDiagnosticsModel$new()

    out <- suppressMessages(suppressWarnings({
        check_api(model = mock_model)
    }))

    expect_type(out, "list")
    expect_equal(out$host, "mock-api.com")
})
