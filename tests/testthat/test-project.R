context("Test project function")

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
    expect_equal_to_reference(pred_1, file = "rds/pred_1.rds")

})





test_that("Test that dates start when needed", {
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
    expect_equal(max(i$dates) + 1, min(get_dates(pred_1)))

})




test_that("Errors are thrown when they should", {
    expect_error(project(NULL),
                 "x is not an incidence object")

    i <- incidence(1:10, 3)
    expect_error(project(i),
                 "daily incidence needed, but interval is 3 days")

    i <- incidence(1:10, 1, group = letters[1:10])
    expect_error(project(i),
                 "cannot use multiple groups in incidence object")

    i <- incidence(1)
    si <- distcrete("gamma", interval = 5L,
                    shape = 1.5,
                    scale = 2, w = 0)
    expect_error(project(i, 1, si = si),
                 "interval used in si is not 1 day, but 5")
    expect_error(project(i, -1, si = si),
                 "R < 0 (value: -1.00)", fixed = TRUE)
    expect_error(project(i, Inf, si = si),
                 "R is not a finite value", fixed = TRUE)
    expect_error(project(i, "tamere", si = si),
                 "R is not numeric", fixed = TRUE)


})
