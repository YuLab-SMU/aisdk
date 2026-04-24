library(testthat)
library(aisdk)
pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)

test_that("Variable Registry works", {
    # Clear registry
    sdk_clear_protected_vars()

    expect_false(sdk_is_var_locked("my_var"))
    expect_null(sdk_get_var_metadata("my_var"))

    sdk_protect_var("my_var", locked = TRUE, cost = "Very High")

    expect_true(sdk_is_var_locked("my_var"))
    meta <- sdk_get_var_metadata("my_var")
    expect_equal(meta$cost, "Very High")
    expect_true(meta$locked)

    sdk_unprotect_var("my_var")
    expect_false(sdk_is_var_locked("my_var"))
})

test_that("AST checker blocks protected variables", {
    sdk_clear_protected_vars()
    sdk_protect_var("expensive_df", locked = TRUE, cost = "High")

    expect_error(
        check_ast_safety("expensive_df <- data.frame()"),
        "Variable 'expensive_df' is protected"
    )

    expect_error(check_ast_safety("expensive_df$new_col <- 1"))

    expect_error(check_ast_safety("expensive_df[['col']] <- 2"))

    expect_error(check_ast_safety("assign('expensive_df', 1)"))

    # Other variables are fine
    expect_silent(check_ast_safety("other_df <- 1"))
})

test_that("list_session_variables shows protection metadata", {
    # Set up
    sdk_clear_protected_vars()
    env <- new.env()
    assign("my_normal_var", 1:10, envir = env)
    assign("my_expensive_model", list(a = 1, b = 2), envir = env)

    sdk_protect_var("my_expensive_model", locked = TRUE, cost = "GPU High")

    # We need to test the tool execution
    agent <- create_coder_agent()
    list_tool <- NULL
    for (t in agent$tools) {
        if (t$name == "list_session_variables") list_tool <- t
    }

    res <- list_tool$run(list(.envir = env))

    expect_true(grepl("my_normal_var", res))
    expect_true(grepl("integer", res))
    expect_true(grepl("my_expensive_model", res))
    expect_true(grepl("PROTECTED: Cost GPU High", res))
    expect_true(grepl("list", res))
})
