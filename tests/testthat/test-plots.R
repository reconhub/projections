context("Test project function")

test_that("Test against reference results", {
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
  plot_1 <- plot(pred_1)
  
  ## Uncomment to generate references
  # library(svglite)
  # ggplot2::ggsave('../figs/test-project-function/basic-example-plot.svg', plot=plot_1)
  
  vdiffr::expect_doppelganger("basic example plot", plot_1)


  ## using simulated ebola data

  si <- distcrete::distcrete(
    "gamma",
    interval = 1L,
    shape = 2.4,
    scale = 4.7,
    w = 0.5)

  i <- incidence::incidence(outbreaks::ebola_sim$linelist$date_of_onset)

  ## add projections after the first 100 days, over 60 days
  set.seed(1)
  proj <- project(x = i[1:100], R = 1.4, si = si, n_days = 60)

  ## plotting projections

  ## Uncomment to generate references
  # ggplot2::ggsave("../figs/test-project-function/evd-proj.svg", plot=plot(proj))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-box-no-outliers.svg",
  #   plot=plot(proj, boxplots = TRUE, outliers = FALSE))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-no-ribbon.svg",
  #   plot=plot(proj, ribbon = FALSE))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-no-box-custom-lines.svg",
  #   plot=plot(proj, boxplots = FALSE,
  #     linetype = 2, linesize = 3))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-red-box.svg",
  #   plot=plot(proj, boxplots = TRUE, boxplots_color = "red"))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-box-only.svg",
  #   plot=plot(proj, quantiles = FALSE,
  #     ribbon = FALSE, boxplots = TRUE))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-ribbon-only.svg",
  #   plot=plot(proj, quantiles = FALSE))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-red-ribbon.svg",
  #   plot=plot(proj, ribbon_color = "red",
  #     quantiles = FALSE))
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-full-red-ribbon-narrow-range.svg",
  #   plot=plot(proj, ribbon_color = "red",
  #     ribbon_alpha = 1, quantiles = FALSE,
  #     ribbon_quantiles = c(.4, .6)))    

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


  ## adding projections to incidence::incidence plot
  p <- plot(i) %>% add_projections(proj)
  
  ## Uncomment to generate references
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-with-incidence-incidence.svg", plot=p)
  
  vdiffr::expect_doppelganger("EVD proj with incidence::incidence", p)
  p <- plot(i) %>% add_projections(proj, boxplots = TRUE)
  
  ## Uncomment to generate references
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-with-incidence-incidence-no-box.svg", 
  #   plot=p)
  
  vdiffr::expect_doppelganger("EVD proj with incidence::incidence no box", p)
  p <- plot(i) %>% add_projections(proj, quantiles = FALSE, ribbon = FALSE,
                                   boxplots = TRUE)
  
  ## Uncomment to generate references
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-with-incidence-incidence-box-only.svg",
  #   plot=p)
  
  vdiffr::expect_doppelganger("EVD proj with incidence::incidence box only", p)

  ## same, custom colors and quantiles
  quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
  pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
  p <- plot(i[1:200]) %>%
    add_projections(proj, quantiles, palette = pal) +
    ggplot2::scale_x_date(date_labels = "%b %Y")

  ## Uncomment to generate references
  # ggplot2::ggsave("../figs/test-project-function/evd-proj-with-incidence-incidence-and-custom.svg",
  #   plot=p)
  
  vdiffr::expect_doppelganger("EVD proj with incidence::incidence and custom", p)

})





test_that("Plotting issues expected errors", {
  skip_on_cran()


  ## simulate basic epicurve
  dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
  i <- incidence::incidence(dat)
  p <- plot(i)
  
  ## example with a function for SI
  expect_error(add_projections(p, "toto"),
               "`x` must be a 'projections' object but is a `character`",
               fixed = TRUE)
})
