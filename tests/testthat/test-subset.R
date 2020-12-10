test_that("Test subsetting with numeric dates inputs", {
    skip_on_cran()

    ## simulate basic epicurve
    dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma", interval = 1L,
                               shape = 1.5,
                               scale = 2, w = 0)
    
    x <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)

    subset_1 <- subset(x, from = 15, to = 20, sim = 1:10)
    ref_1 <- x[get_dates(x) %in% 15:20, 1:10]
    expect_identical(ref_1, subset_1)
    
    subset_2 <- subset(x, from = 15, sim = c(TRUE, FALSE))
    ref_2 <- x[get_dates(x) >= 15, c(TRUE, FALSE)]
    expect_identical(ref_2, subset_2)
    
    subset_3 <- subset(x, to = 15, sim = c(TRUE, FALSE))
    ref_3 <- x[get_dates(x) <= 15, c(TRUE, FALSE)]
    expect_identical(ref_3, subset_3)

    expect_identical(x[], x)
    expect_identical(as.vector(subset_1),
                     unname(unlist(as.data.frame(x)[get_dates(x) %in% 15:20, 2:11])))

    expect_error(subset(x, from = 1, to = 0), "No data retained.")

})





test_that("Test subsetting with Date inputs", {
    skip_on_cran()

    ## simulate basic epicurve
    day <- as.Date("1982-01-01")
    dat <- day + c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
    i <- incidence::incidence(dat)


    ## example with a function for SI
    si <- distcrete::distcrete("gamma", interval = 1L,
                    shape = 1.5,
                    scale = 2, w = 0)

    x <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)

    subset_1 <- subset(x, from = day + 15, to = day + 20, sim = 1:10)
    ref_1 <- x[get_dates(x) %in% (day + 15:20), 1:10]
    expect_identical(ref_1, subset_1)
    
    subset_2 <- subset(x, from = day + 15, sim = c(TRUE, FALSE))
    ref_2 <- x[get_dates(x) >= (day + 15), c(TRUE, FALSE)]
    expect_identical(ref_2, subset_2)
    
    subset_3 <- subset(x, to = day + 15, sim = 3:10)
    ref_3 <- x[get_dates(x) <= (day + 15), 3:10]
    expect_identical(ref_3, subset_3)

})





test_that("Test [ operator handles dates and content", {
  skip_on_cran()

  ## test with numeric dates
  dates <- 0:9
  x_mat <- matrix(1:30, ncol = 3)
  x <- build_projections(x_mat, dates = dates)
  sub_x <- x[4:2, 2:3]
  expect_identical(as.vector(x_mat[4:2, 2:3]), as.vector(sub_x))
  expect_identical(dates[4:2], get_dates(sub_x))

  ## test with Date
  dates <- 0:9 + Sys.Date()
  x_mat <- matrix(1:30, ncol = 3)
  x <- build_projections(x_mat, dates = dates)
  sub_x <- x[4:2, 2:3]
  expect_identical(as.vector(sub_x), as.vector(x_mat[4:2, 2:3]))
  expect_identical(dates[4:2], get_dates(sub_x))

})
