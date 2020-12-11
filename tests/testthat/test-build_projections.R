test_that("Test round trip", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)

    set.seed(1)
    pred_1 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)
    colnames(pred_1) <- paste("sim", 1:ncol(pred_1), sep = "_")

    df <- as.data.frame(pred_1)
    new_pred <- build_projections(df[, -1], df[, 1])
    expect_identical(pred_1, new_pred)

})



test_that("Test dates default", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)

    set.seed(1)
    x <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)

    df <- as.data.frame(x)
    new_x <- build_projections(df[, -1])
    expect_equal(seq_along(get_dates(x)) - 1L, get_dates(new_x))
})



test_that("Test errors", {
    skip_on_cran()

    expect_error(
      new_projections(matrix(1:10, ncol = 2), dates = 1:10, cumulative = FALSE),
      "Number of dates (10) does not match number of rows (5)",
      fixed = TRUE)

    expect_error(
      build_projections(matrix(1:10, ncol = 2), dates = 1:10),
      "Number of dates (10) does not match number of rows (5)",
      fixed = TRUE)

})





test_that("Test ordering", {
    skip_on_cran()

    mat <- matrix(round(rnorm(100, 10)), ncol = 20)
    dates <- Sys.Date() + c(5, 1, 3, 2, 4)

    ## test ordering
    x <- build_projections(mat, dates)
    expect_identical(get_dates(x), sort(dates))
    expect_equal(as.vector(mat[order(dates), ]), as.vector(x))

    ## test no ordering
    x <- build_projections(mat, dates, order_dates = FALSE)
    expect_identical(get_dates(x), dates)
    expect_equal(as.vector(mat), as.vector(x))
    
})

