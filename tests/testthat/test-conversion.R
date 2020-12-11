test_that("Test against reference results", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- as.Date("2001-01-01") + c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma",
                               interval = 1L,
                               shape = 1.5,
                               scale = 2, w = 0)

    x <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)

       
    ## basic export
    df_1 <- as.data.frame(x)
    expect_identical(get_dates(x), df_1$dates)
    expect_identical(as.vector(x), unname(unlist(df_1[-1])))

    
    ## long format
    df_2 <- as.data.frame(x, long = TRUE)
    expect_identical(3L, ncol(df_2))
    expect_identical(c("date", "incidence", "sim"), names(df_2))
    expect_identical(get_dates(x), unique(df_2$date))
    expect_identical(as.vector(x), unname(unlist(df_2[[2]])))
    expect_identical(ncol(x), length(unique(df_2[[3]])))
})


