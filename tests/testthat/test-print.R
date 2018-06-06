context("Test printing")

test_that("Printing as planned", {
  skip_on_cran()

  ## simulate basic epicurve
  dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
  i <- incidence(dat)


  ## example with a function for SI
  si <- distcrete("gamma", interval = 1L,
                  shape = 1.5,
                  scale = 2, w = 0)

  set.seed(1)
  x <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)
  y <- cumulate(x)

  expect_equal_to_reference(capture.output(print(x)),
                            file = "rds/print_x.rds")
  expect_equal_to_reference(capture.output(print(y)),
                            file = "rds/print_y.rds")

})
