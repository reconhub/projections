context("Test project function")

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
  plot_1 <- plot(pred_1)
  vdiffr::expect_doppelganger("basic example plot", plot_1)


  ## using simulated ebola data

  si <- distcrete("gamma", interval = 1L,
                  shape = 0.37,
                  scale = 41.4, w = 0)

  i <- incidence(ebola_sim$linelist$date_of_onset)

  ## add projections after the first 100 days, over 60 days
  set.seed(1)
  proj <- project(x = i[1:100], R = 2.1, si = si, n_days = 60)

  ## plotting projections
  vdiffr::expect_doppelganger("EVD proj", plot(proj))
  vdiffr::expect_doppelganger("EVD proj box, no outliers",
                              plot(proj, boxplots = TRUE, outliers = FALSE))
  vdiffr::expect_doppelganger("EVD proj no ribbon",
                              plot(proj, ribbon = FALSE))
  vdiffr::expect_doppelganger("EVD proj no box custom lines",
                              plot(proj, boxplots = FALSE,
                                   linetype = 2, linesize = 3))
  vdiffr::expect_doppelganger("EVD proj red box",
                              plot(proj, boxplots = TRUE, boxplots_color = "red"))
  vdiffr::expect_doppelganger("EVD proj box only",
                              plot(proj, quantiles = FALSE,
                                   ribbon = FALSE, boxplots = TRUE))
  vdiffr::expect_doppelganger("EVD proj ribbon only",
                              plot(proj, quantiles = FALSE))
  vdiffr::expect_doppelganger("EVD proj red ribbon",
                              plot(proj, ribbon_color = "red",
                                   quantiles = FALSE))
  vdiffr::expect_doppelganger("EVD proj full red ribbon narrow range",
                              plot(proj, ribbon_color = "red",
                                   ribbon_alpha = 1, quantiles = FALSE,
                                   ribbon_quantiles = c(.4, .6)))


  ## adding projections to incidence plot
  p <- plot(i) %>% add_projections(proj)
  vdiffr::expect_doppelganger("EVD proj with incidence", p)
  p <- plot(i) %>% add_projections(proj, boxplots = TRUE)
  vdiffr::expect_doppelganger("EVD proj with incidence no box", p)
  p <- plot(i) %>% add_projections(proj, quantiles = FALSE, ribbon = FALSE,
                                   boxplots = TRUE)
  vdiffr::expect_doppelganger("EVD proj with incidence box only", p)

  ## same, custom colors and quantiles
  quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
  pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
  p <- plot(i[1:200]) %>%
    add_projections(proj, quantiles, palette = pal)

  vdiffr::expect_doppelganger("EVD proj with incidence and custom", p)

})


