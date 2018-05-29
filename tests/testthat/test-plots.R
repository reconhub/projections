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
  if (require(outbreaks) &&
        require(distcrete) &&
        require(incidence) &&
        require(magrittr)) {

    si <- distcrete("gamma", interval = 1L,
                    shape = 0.37,
                    scale = 41.4, w = 0)

    i <- incidence(ebola_sim$linelist$date_of_onset)
    plot(i)

    ## add projections after the first 100 days, over 60 days
    set.seed(1)
    proj <- project(x = i[1:100], R = 2.1, si = si, n_days = 60)

    ## plotting projections
    vdiffr::expect_doppelganger("projections using EVD", plot(proj))

    ## adding projections to incidence plot
    p <- plot(i) %>% add_projections(proj)
    vdiffr::expect_doppelganger("projections using EVD with incidence", p)

    ## same, custom colors and quantiles
    quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
    pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
    p <- plot(i[1:200]) %>%
      add_projections(proj, quantiles, palette = pal)

    vdiffr::expect_doppelganger("projections using EVD with incidence and custom", p)
  }
})


