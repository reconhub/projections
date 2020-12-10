test_that("Projections can be performed for a single day", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 2
  
  p <- project(x = i,
               si = si,
               R = R0,
               n_sim = 2,  # doesn't work with 1 in project function
               R_fix_within = TRUE,
               n_days = 1, # doing 2 days as project function currently not working with one day - will only use first day though
               model = "poisson"
               )

  expect_identical(get_dates(p), as.Date("2020-01-24"))
})





test_that("Projections can be performed for a single day", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 2
  
  p <- project(x = i,
               si = si,
               R = R0,
               n_sim = 1,  # doesn't work with 1 in project function
               R_fix_within = TRUE,
               n_days = 2, # doing 2 days as project function currently not working with one day - will only use first day though
               model = "poisson"
               )

  expect_identical(get_dates(p), as.Date("2020-01-24") + 0:1)
  expect_identical(ncol(p), 1L)
})





test_that("Projections can be performed for a single day and single simulation", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 2
  
  p <- project(x = i,
               si = si,
               R = R0,
               n_sim = 1,  # doesn't work with 1 in project function
               R_fix_within = TRUE,
               n_days = 1, # doing 2 days as project function currently not working with one day - will only use first day though
               model = "poisson"
               )

  expect_identical(get_dates(p), as.Date("2020-01-24"))
  expect_identical(ncol(p), 1L)
})





test_that("Test that dates start when needed", {
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
  expect_equal(max(i$dates) + 1, min(get_dates(pred_1)))

})





test_that("Errors are thrown when they should", {
  expect_error(project(NULL),
               "x is not an incidence object")

  i <- incidence::incidence(1:10, 3)
  expect_error(project(i),
               "daily incidence needed, but interval is 3 days")

  i <- incidence::incidence(1:10, 1, group = letters[1:10])
  expect_error(project(i),
               "cannot use multiple groups in incidence object")
  i <- incidence::incidence(seq(Sys.Date(), by = "month", length.out = 12), "month")
  expect_error(project(i),
               "daily incidence needed, but interval is 30 days")

  i <- incidence::incidence(1)
  si <- distcrete::distcrete("gamma", interval = 5L,
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
  expect_error(project(i, R = list(1), si = si, time_change = 2),
               "`R` must be a `list` of size 2 to match 1 time changes; found 1",
               fixed = TRUE)
  expect_error(project(i, si = si, time_change = "pophip"),
               "`time_change` must be `numeric`, but is a `character`",
               fixed = TRUE)
  msg <- ifelse(R.version.string > "3.6.3",
                "`R` must be a `vector` or a `list` if `time_change` provided; it is a `matrix, array`",
                "`R` must be a `vector` or a `list` if `time_change` provided; it is a `matrix`")
  expect_error(project(i, si = si, time_change = 2, R = matrix(1.2)),
               msg,
               fixed = TRUE)
  
})





test_that("Test against reference results - Poisson model", {
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
  expect_snapshot_value(pred_1, style = "serialize")
  

  ## time-varying R (fixed within time windows)
  set.seed(1)
  pred_2 <- project(i,
                    R = c(1.5, 0.5, 2.1, .4, 1.4),
                    si = si,
                    n_days = 60,
                    time_change = c(10, 15, 20, 30),
                    n_sim = 100)
  expect_snapshot_value(pred_2, style = "serialize")


  ## time-varying R, 2 periods, R is 2.1 then 0.5
  set.seed(1)

  pred_3 <- project(i,
                    R = c(2.1, 0.5),
                    si = si,
                    n_days = 60,
                    time_change = 40,
                    n_sim = 100)
  expect_snapshot_value(pred_3, style = "serialize")

  ## time-varying R, 2 periods, separate distributions of R for each period
  set.seed(1)
  R_period_1 <- runif(100, min = 1.1, max = 3)
  R_period_2 <- runif(100, min = 0.6, max = .9)

  pred_4 <- project(i,
                    R = list(R_period_1, R_period_2),
                    si = si,
                    n_days = 60,
                    time_change = 20,
                    n_sim = 100)
  expect_snapshot_value(pred_4, style = "serialize")

})





test_that("Test against reference results - NegBin model", {
  skip_on_cran()

  ## simulate basic epicurve
  dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
  i <- incidence::incidence(dat)


  ## example with a function for SI
  si <- distcrete::distcrete("gamma", interval = 1L,
                             shape = 1.5,
                             scale = 2, w = 0)

  set.seed(1)
  pred_5 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30, model = "negbin")
  expect_snapshot_value(pred_5, style = "serialize")


  ## time-varying R (fixed within time windows)
  set.seed(1)
  pred_6 <- project(i,
                    R = c(1.5, 0.5, 2.1, .4, 1.4),
                    si = si,
                    n_days = 60,
                    time_change = c(10, 15, 20, 30),
                    n_sim = 100,
                    model = "negbin")
  expect_snapshot_value(pred_6, style = "serialize")


  ## time-varying R, 2 periods, R is 2.1 then 0.5
  set.seed(1)

  pred_7 <- project(i,
                    R = c(2.1, 0.5),
                    si = si,
                    n_days = 60,
                    time_change = 40,
                    n_sim = 100,
                    model = "negbin")
  expect_snapshot_value(pred_7, style = "serialize")

  ## time-varying R, 2 periods, separate distributions of R for each period
  set.seed(1)
  R_period_1 <- runif(100, min = 1.1, max = 3)
  R_period_2 <- runif(100, min = 0.6, max = .9)

  pred_8 <- project(i,
                    R = list(R_period_1, R_period_2),
                    si = si,
                    n_days = 60,
                    time_change = 20,
                    n_sim = 100,
                    model = "negbin")
  expect_snapshot_value(pred_8, style = "serialize")

})





test_that("Test R_fix_within", {
  
  ## The rationale of this test is to check that the variance of trajectories
  ## when fixing R within a given simulation is larger than when drawing
  ## systematically from the distribution. On the provided example, fixing R
  ## will lead to many more trajectories growing fast, and greater average
  ## incidence (> x10 for the last time steps).
  
  skip_on_cran()
  
  ## simulate basic epicurve
  dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
  i <- incidence::incidence(dat)

  set.seed(1)
  x_base <- project(i,
                    si = c(1, 1 , 1, 1),
                    R = c(0.8, 1.2),
                    n_days = 50,
                    n_sim = 1000,
                    R_fix_within = FALSE)
  x_fixed <- project(i,
                     si = c(1, 1 , 1, 1),
                     R = c(0.8, 1.2),
                     n_days = 50,
                     n_sim = 1000,
                     R_fix_within = TRUE)
  expect_true(all(tail(rowSums(x_fixed) / rowSums(x_base), 5) > 10))
  
})
