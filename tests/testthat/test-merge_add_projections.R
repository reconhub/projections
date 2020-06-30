context("Test merge_add_projections")

test_that("Merging works", {

  set.seed(1)
  i <- incidence::incidence(as.Date('2020-01-01') + sample(1:30, 10))
  si <- c(0.2, 0.5, 0.2, 0.1)
  
  x_1 <- project(x = i[1:10],
                 si = si,
                 R = 2,
                 n_sim = 1000,
                 R_fix_within = TRUE,
                 n_days = 10,
                 model = "poisson"
                 )[1:3, ]

  x_2 <- project(x = i,
                 si = si,
                 R = 1.5,
                 n_sim = 100,
                 R_fix_within = TRUE,
                 n_days = 20,
                 model = "poisson"
                 )

  x_3 <- project(x = i,
                 si = si,
                 R = 3.4,
                 n_sim = 200,
                 R_fix_within = TRUE,
                 n_days = 8,
                 model = "poisson"
                 )

  ## test adding these 3 simulations
  list_x <- list(x_1, x_2, x_3)
  x <- merge_add_projections(list_x)


  ## check date range is correct: output should start at first date of all
  ## inputs, end of last dates of all inputs
  
  first_date <- min(Reduce("c", lapply(list_x, function(e) min(get_dates(e)))))
  last_date <- max(Reduce("c", lapply(list_x, function(e) max(get_dates(e)))))
  expect_equal(min(get_dates(x)), first_date)
  expect_equal(max(get_dates(x)), last_date)


  ## check dimensions: output should have as many sims as the largest input
  
  n_sims <- max(sapply(list_x, ncol))
  expect_equal(ncol(x), n_sims)


  ## check values
  x <- merge_add_projections(list(x_2[-c(1:3), ], x_2[c(1:3), ]))
  expect_equal(as.vector(x), as.vector(x_2))

  x <- merge_add_projections(list(x_2, x_2[c(1:3), ]))
  expect_equal(as.vector(x[1:3, ]), as.vector(x_2[1:3, ] * 2))
  expect_equal(as.vector(x[-(1:3), ]), as.vector(x_2[-(1:3), ]))


  ## check operator version
  expect_identical(x + x, x * 2)
  expect_identical(x[-1, ] + x[1, ], x)

  ## check date continuity
  y <- x[1,] + x[nrow(x),]
  expected_dates <- seq(from = min(get_dates(x)), to = max(get_dates(x)), by = 1L)
  expect_identical(get_dates(y), expected_dates)
  
})





test_that("Errors are issued as they should", {

  msg <- "x is not a `list` but a character"
  expect_error(merge_add_projections(letters), msg)

  msg <- "some input objects are not `projections` objects"
  expect_error(merge_add_projections(list(letters)), msg)

  msg <- "x is an empty `list`"
  expect_error(merge_add_projections(list()), msg)

})





test_that("+ operator works with numeric right-hand operator", {

  set.seed(1)
  i <- incidence::incidence(as.Date('2020-01-01') + sample(1:30, 10))
  si <- c(0.2, 0.5, 0.2, 0.1)
  
  x <- project(x = i[1:10],
                 si = si,
                 R = 2,
                 n_sim = 1000,
                 R_fix_within = TRUE,
                 n_days = 10,
                 model = "poisson"
                 )[1:3, ]

  ## test with a scalar integer
  x_plus <- x + 2L
  expect_identical(as.matrix(x) + 2L, as.matrix(x_plus))
  expect_identical(class(x), class(x_plus))

  ## test with a with a numeric vector
  x_plus <- x + 10:13
  expect_identical(as.matrix(x) + 10:13, as.matrix(x_plus))
  expect_identical(class(x), class(x_plus))

  ## test with a with a decimal numbers
  b <- 10:13 + 1.1
  x_plus <- x + b
  expect_identical(as.matrix(x) + b, as.matrix(x_plus))
  expect_identical(class(x), class(x_plus))

})
