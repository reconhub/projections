save_svg <- function(plot) {
  path <- tempfile(fileext = ".svg")
  ggplot2::ggsave(path, plot = plot)
  path
}

expect_snapshot_plot <- function(code, name) {
  # Other packages might affect results
  skip_if_not_installed("svglite")
  path <- save_svg(code)
  expect_snapshot_file(path, paste0(name, ".svg"))
}

test_that("Test against reference results", {
  skip_on_ci()

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

  expect_snapshot_plot(plot_1, "basic-example-plot")

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
  plot_2 <- plot(proj)
  expect_snapshot_plot(plot_2, "evd-proj")

  plot_3 <- plot(proj, boxplots = TRUE, outliers = FALSE)
  expect_snapshot_plot(plot_3, "evd-proj-box-no-outliers")

  plot_4 <- plot(proj, ribbon = FALSE)
  expect_snapshot_plot(plot_4, "evd-proj-no-ribbon")

  plot_5 <- plot(proj, boxplots = FALSE, linetype = 2, linesize = 3)
  expect_snapshot_plot(plot_5, "evd-proj-no-box-custom-lines")

  plot_6 <- plot(proj, boxplots = TRUE, boxplots_color = "red")
  expect_snapshot_plot(plot_6, "evd-proj-red-box")

  plot_7 <- plot(proj, quantiles = FALSE, ribbon = FALSE, boxplots = TRUE)
  expect_snapshot_plot(plot_7, "evd-proj-box-only")

  plot_8 <- plot(proj, quantiles = FALSE)
  expect_snapshot_plot(plot_8, "evd-proj-ribbon-only")

  plot_9 <- plot(proj, ribbon_color = "red", quantiles = FALSE)
  expect_snapshot_plot(plot_9, "evd-proj-red-ribbon")

  plot_10 <- plot(
    proj,
    ribbon_color = "red",
    ribbon_alpha = 1,
    quantiles = FALSE,
    ribbon_quantiles = c(.4, .6)
  )
  expect_snapshot_plot(plot_10, "evd-proj-full-red-ribbon-narrow-range")


  ## adding projections to incidence::incidence plot
  plot_11 <- plot(i) %>% add_projections(proj)
  expect_snapshot_plot(plot_11, "evd-proj-with-incidence-incidence")

  plot_12 <- plot(i) %>% add_projections(proj, boxplots = TRUE)
  expect_snapshot_plot(plot_12, "evd-proj-with-incidence-incidence-no-box")

  plot_13 <-
    plot(i) %>%
    add_projections(proj, quantiles = FALSE, ribbon = FALSE, boxplots = TRUE)
  expect_snapshot_plot(plot_13, "evd-proj-with-incidence-incidence-box-only")


  ## same, custom colors and quantiles
  quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
  pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
  plot_14 <- plot(i[1:200]) %>%
    add_projections(proj, quantiles, palette = pal) +
    ggplot2::scale_x_date(date_labels = "%b %Y")
  expect_snapshot_plot(plot_14, "evd-proj-with-incidence-incidence-and-custom")

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
