context("Test conversion")

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

    ## basic export
    df_1 <- as.data.frame(pred_1)
    expect_equal_to_reference(df_1, file = "rds/df_1.rds")
    expect_equal(as.vector(unlist(df_1[, -1])), as.vector(pred_1))

    ## long format
    df_2 <- as.data.frame(pred_1, long = TRUE)
    expect_equal_to_reference(df_2, file = "rds/df_2.rds")
   
    
})


