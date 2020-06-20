context("Test conversion")

setup(RNGversion("3.5.3"))
teardown({
  cur_R_version <- trimws(substr(R.version.string, 10, 16))
  RNGversion(cur_R_version)
})

test_that("Test against reference results", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- as.Date("2001-01-01") + c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)

    set.seed(1)
    pred_1 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)

    ## basic export
    df_1 <- as.data.frame(pred_1)
    ## Uncomment to generate reference
    # saveRDS(df_1, file = "rds/df_1.rds")
    expect_equal_to_reference(df_1, file = "rds/df_1.rds", update = FALSE)
    expect_equal(as.vector(unlist(df_1[, -1])), as.vector(pred_1))

    ## long format
    df_2 <- as.data.frame(pred_1, long = TRUE)
    ## Uncomment to generate reference
    # saveRDS(df_2, file = "rds/df_2.rds")
    expect_equal_to_reference(df_2, file = "rds/df_2.rds", update = FALSE)


})


