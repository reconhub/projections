#' Plot projections objects
#'
#' This method is designed for plotting \code{projections} objects, output by
#' the function \code{\link{project}}. It plots the mean projected incidence, as
#' well as lower and upper bounds defined by quantiles.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#'
#' @param x A \code{projections} object.
#'
#' @param y A vector of 2 quantiles to plot
#'
#' @param col A vector (recycled if needed) of up to 3 colors for the median,
#'   lower and upper quantiles.
#'
#' @param ... Further arguments to be passed to other methods (not used).
#'
#' @examples
#'
#' if (require(outbreaks) &&
#'     require(distcrete) &&
#'     require(incidence)) {
#'
#' si <- distcrete("gamma", interval = 1L,
#'                  shape = 0.37,
#'                  scale = 41.4, w = 0)
#'
#' i <- incidence(ebola_sim$linelist$date_of_onset)
#' plot(i)
#'
#' ## add projections after the first 100 days, over 60 days
#' set.seed(1)
#' proj <- project(x = i[1:100], R = 2.1, si = si, n_days = 60)
#'
#' ## plotting projections
#' plot(proj)
#'
#' ## adding them to incidence plot
#' plot(i[1:160], proj = proj)
#'
#' ## same, with customised colors
#' plot(i[1:160], proj = plot(proj, col = c("red", "gold", "gold")))
#'
#' }
#'

plot.projections <- function(x, y = c(0.01, 0.05, 0.5, 0.95, 0.99),
                             palette = quantile_pal, ...) {
  y <- sort(y)

  stats <- t(apply(x, 1, stats::quantile, y))
  dates <- attr(x, "dates")
  df <- cbind.data.frame(dates = rep(dates, ncol(stats)),
                         quantile = rep(colnames(stats), each = nrow(stats)),
                         value = as.vector(stats))

  colors <- color_quantiles(df$quantile, palette)

  out <- ggplot2::ggplot(df, ggplot2::aes_string(x = "dates")) +
    ggplot2::geom_line(aes_string(y = "value", color = "quantile")) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "", y = "Predicted incidence")

  out
}
