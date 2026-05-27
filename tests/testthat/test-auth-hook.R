test_that("request_authorization handles green risk", {
    expect_true(request_authorization("some safe action", "GREEN"))
})

test_that("request_authorization strictly blocks red risk", {
    expect_error(
        request_authorization("some dangerous action", "RED"),
        "Action Rejected \\(RED Risk\\)"
    )
})

test_that("request_authorization blocks yellow risk in non-interactive tests", {
    # Since testthat runs non-interactively, Yellow risk should immediately abort
    # with the 'interactive authorization required...' message.
    expect_error(
        request_authorization("some file operation", "YELLOW"),
        "Action Rejected: Interactive authorization required"
    )
})
