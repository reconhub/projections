context("Test subset")

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

    subset_1 <- subset(pred_1, from = 15, to = 20, sim = 1:10)
    subset_2 <- subset(pred_1, from = 15, sim = c(TRUE, FALSE))

    expect_equal_to_reference(subset_1, file = "rds/subset_1.rds")
    expect_equal_to_reference(subset_2, file = "rds/subset_2.rds")

})

