context("Test print function")

test_that("Test against reference results", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence(dat)


    ## example with a function for SI
    si <- distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)

    set.seed(1)
    pred_1 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)
    expect_equal_to_reference(capture.output(print(pred_1)),
                              file = "rds/print_1.rds")
    
})


