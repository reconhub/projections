context("Test printing")

setup(RNGversion("3.5.3"))
teardown({
  cur_R_version <- trimws(substr(R.version.string, 10, 16))
  RNGversion(cur_R_version)
})

test_that("Printing as planned", {
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
  y <- incidence::cumulate(x)

  expect_identical(apply(x, 2, cumsum), unclass(as.matrix(y)))
  expect_error(cumulate(y), "x is already a cumulative incidence")

})
