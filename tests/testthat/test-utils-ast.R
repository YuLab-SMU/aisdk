test_that("check_ast_safety block banned functions", {
    # system
    expect_error(check_ast_safety('system("ls")'), "AST Safety Violation: Call to strictly prohibited function 'system' is not allowed.")
    expect_error(check_ast_safety('base::system("ls")'), "AST Safety Violation: Call to strictly prohibited function 'system' is not allowed.")

    # eval
    expect_error(check_ast_safety('eval(parse(text="system(\'ls\')"))'), "AST Safety Violation: Call to strictly prohibited function 'eval' is not allowed.")
    expect_error(check_ast_safety('eval("1+1")'), "AST Safety Violation: Call to strictly prohibited function 'eval' is not allowed.")

    # file ops
    expect_error(check_ast_safety('file.remove("x")'), "Action Rejected: Interactive authorization required")

    # <<-
    expect_error(check_ast_safety("x <<- 1"), "Action Rejected: Interactive authorization required")
})

test_that("check_ast_safety block global environment assign", {
    # global target assignment
    expect_error(check_ast_safety('assign("x", 1, envir = globalenv())'), "Action Rejected: Interactive authorization required")
    expect_error(check_ast_safety('assign("a", 2, envir = .GlobalEnv)'), "Action Rejected: Interactive authorization required")

    expect_error(check_ast_safety('assign("y", 1, pos = 1, envir = globalenv())'), "Action Rejected: Interactive authorization required")

    # 3rd positional argument mapping to envir
    expect_error(check_ast_safety('assign("y", 1, globalenv())'), "Action Rejected: Interactive authorization required")

    # safe assign is allowed
    expect_silent(check_ast_safety('assign("x", 1)'))
    expect_silent(check_ast_safety('assign("x", 1, envir = new.env())'))
})

test_that("check_ast_safety allows safe functions", {
    # Basics
    expect_silent(check_ast_safety("x <- 1 + 1"))

    # dplyr and purrr
    expect_silent(check_ast_safety("library(dplyr); mtcars %>% filter(mpg > 20)"))

    # string ops
    expect_silent(check_ast_safety('gsub("a", "b", "abc")'))

    # Function definitions
    expect_silent(check_ast_safety("f <- function(y) { return(y * 2) }; f(2)"))
})

test_that("walk_ast works properly", {
    nodes <- character()
    walk_ast(parse(text = "1+2"), function(n) {
        if (is.symbol(n)) nodes <<- c(nodes, as.character(n))
    })
    expect_true("+" %in% nodes)
})
