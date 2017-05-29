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

plot.projections <- function(x, y = c(0.05, 0.95), col = "red", ...) {
  if (length(y) != 2L) {
    msg <- "y should have two quantile values"
    stop(msg)
  }

  y <- sort(y)
  col <- rep(col, length = 3)

  get_stats <- function(v) {
    c(median = stats::median(v),
      stats::quantile(v, y))
  }

  dates <- attr(x, "dates")
  stats <- t(apply(x, 1, get_stats))
  colnames(stats) <- c("median", "lwr", "upr")
  df <- cbind.data.frame(dates, stats)


  out <- ggplot2::ggplot(df, ggplot2::aes_string(x = "dates")) +
    ggplot2::geom_line(ggplot2::aes_string(y = "median"),
                       linetype = 1, color = col[1]) +
    ggplot2::geom_line(ggplot2::aes_string(y = "lwr"),
                       linetype = 2, color = col[2]) +
    ggplot2::geom_line(ggplot2::aes_string(y = "upr"),
                       linetype = 2, color = col[3]) +
    ggplot2::labs(y = "Predicted incidence")

  out
}
