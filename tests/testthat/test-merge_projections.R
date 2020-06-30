context("Test merge_projections")

test_that("Merging works", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 10
  
  x <- project(x = i,
               si = si,
               R = R0,
               n_sim = 2,
               R_fix_within = TRUE,
               n_days = 10,
               model = "poisson"
               )


  ## test basic merge
  x_2 <- merge_projections(list(x, x))
  expect_equal(as.vector(x_2[, 1:2]), as.vector(x))


  ## different dates, 3 objects
  x_3 <- merge_projections(list(x, x[1:3, 1], x[5:6, 1:2]))
  expect_equal(ncol(x_3), ncol(x) + 3)
  expect_equal(as.vector(x_3[, 1:2]), as.vector(x))
  expect_equal(as.vector(x_3[1:3, 3]), as.vector(x[1:3, 1]))
  expect_equal(as.vector(x_3[5:6, 4:5]), as.vector(x[5:6, 1:2]))
  expect_equal(get_dates(x), get_dates(x_3)) # check dates
  expect_true(all(x_3[-c(1:3), 3] == 0)) # check zeros in right place
})





test_that("Errors are issued as they should", {

  msg <- "x is not a `list` but a character"
  expect_error(merge_projections(letters), msg)

  msg <- "some input objects are not `projections` objects"
  expect_error(merge_projections(list(letters)), msg)

  msg <- "x is an empty `list`"
  expect_error(merge_projections(list()), msg)

})
