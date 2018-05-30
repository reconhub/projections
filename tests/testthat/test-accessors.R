context("Test accessors")

test_that("Accessors return the right thing", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence(dat)

    ## example with a function for SI
    si <- distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)


    pred_1 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)
    expect_equal(attr(pred_1, "dates"), get_dates(pred_1))

})


test_that("Expected errors", {
    skip_on_cran()

    expect_error(get_dates("toto"),
                 "Not implemented for class character")
})
