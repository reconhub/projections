context("Test summary of projections objects")

test_that("Testing default summary", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 2
  
  p <- project(x = i,
               si = si,
               R = R0,
               n_sim = 2, 
               R_fix_within = TRUE,
               n_days = 10,
               model = "poisson"
               )

  s <- summary(p)

  expect_identical(get_dates(p), s$dates)
  expect_equal(as.vector(apply(p, 1, median)), s$`quantiles.50%`)
  expect_equal(as.vector(apply(p, 1, mean)), s$mean)
  expect_equal(as.vector(apply(p, 1, sd)), s$sd)
  expect_equal(as.vector(apply(p, 1, min)), s$min)
  expect_equal(as.vector(apply(p, 1, max)), s$max)
  expect_identical(as.vector(apply(p, 1, quantile, 0.025)),
                   s$`quantiles.2.5%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.975)),
                   s$`quantiles.97.5%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.25)),
                   s$`quantiles.25%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.75)),
                   s$`quantiles.75%`)

})





test_that("Testing summary on/off", {
  i <- incidence::incidence(as.Date('2020-01-23'))
  si <- c(0.2, 0.5, 0.2, 0.1)
  R0 <- 2
  
  p <- project(x = i,
               si = si,
               R = R0,
               n_sim = 2, 
               R_fix_within = TRUE,
               n_days = 10,
               model = "poisson"
               )

  ## no min/max/mean/sd
  s_quantiles_only <- summary(p, min = FALSE, max = FALSE, mean = FALSE, sd = FALSE)
  expect_identical(get_dates(p), s_quantiles_only$dates)
  expect_null(s_quantiles_only$mean)
  expect_null(s_quantiles_only$sd)
  expect_null(s_quantiles_only$min)
  expect_null(s_quantiles_only$max)
  expect_equal(as.vector(apply(p, 1, median)), s_quantiles_only$`quantiles.50%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.025)),
                   s_quantiles_only$`quantiles.2.5%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.975)),
                   s_quantiles_only$`quantiles.97.5%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.25)),
                   s_quantiles_only$`quantiles.25%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.75)),
                   s_quantiles_only$`quantiles.75%`)

  ## no quantiles
  s_no_quantiles <- summary(p, quantiles = FALSE)
  expect_identical(get_dates(p), s_no_quantiles$dates)
  expect_equal(as.vector(apply(p, 1, mean)), s_no_quantiles$mean)
  expect_equal(as.vector(apply(p, 1, sd)), s_no_quantiles$sd)
  expect_equal(as.vector(apply(p, 1, min)), s_no_quantiles$min)
  expect_equal(as.vector(apply(p, 1, max)), s_no_quantiles$max)
  
  expect_identical(c("dates", "mean", "sd", "min", "max"), names(s_no_quantiles))
  expect_identical(s_no_quantiles, summary(p, quantiles = NULL))
  expect_identical(s_no_quantiles, summary(p, quantiles = numeric(0)))
  
  
  ## different set of quantiles
  s_other_quantiles <- summary(p, quantiles = c(0.4, 0.7))
  expect_identical(as.vector(apply(p, 1, quantile, 0.4)),
                   s_other_quantiles$`quantiles.40%`)
  expect_identical(as.vector(apply(p, 1, quantile, 0.7)),
                   s_other_quantiles$`quantiles.70%`)

})
