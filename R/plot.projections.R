#' Plot projections objects
#'
#' The \code{plot} method of \code{projections} objects (output by the function
#' \code{\link{project}}) shows quantiles of predicted incidence over time. The
#' function \code{add_projections} can be used to add a similar plot to an
#' existing \code{incidence} plot. This latter function is piping friendly (see
#' examples).
#'
#' @seealso \code{\link{project}} to generate projections
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' @importFrom graphics plot
#'
#' @aliases plot.projections
#'
#' @param x A \code{projections} object.
#'
#' @param y A vector of quantiles to plot, automatically completed to be
#'   symmetric around the median.
#'
#' @param palette A color palette to be used for plotting the quantile lines;
#'   defaults to \code{quantile_pal}.
#'
#' @param ... Further arguments to be passed to other methods (not used).
#'
#' @examples
#'
#' if (require(outbreaks) &&
#'     require(distcrete) &&
#'     require(incidence) &&
#'     require(magrittr)) {
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
#' plot(i) %>% add_projections(proj)
#' plot(i[1:160]) %>% add_projections(proj)
#'
#' ## same, with customised quantiles and colors
#' quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
#' pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
#' plot(i[1:200]) %>%
#'   add_projections(proj, quantiles, palette = pal)
#'
#' }
#'

plot.projections <- function(x, y = c(0.01, 0.05, 0.1, 0.5),
                             palette = quantile_pal, ...) {
  y <- sort(unique(c(y, 1-y)))
  y <- y[y > 0 & y < 1]

  stats <- t(apply(x, 1, stats::quantile, y))
  dates <- attr(x, "dates")
  quantiles <- rep(colnames(stats), each = nrow(stats))
  quantiles <- factor(quantiles, levels = unique(quantiles))
  df <- cbind.data.frame(dates = rep(dates, ncol(stats)),
                         quantile = quantiles,
                         value = as.vector(stats),
                         stringsAsFactors = FALSE)

  colors <- color_quantiles(df$quantile, palette)

  out <- ggplot2::ggplot(df, ggplot2::aes_string(x = "dates")) +
    ggplot2::geom_line(ggplot2::aes_string(y = "value", color = "quantile")) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "", y = "Predicted incidence")

  out
}





## This function will take an existing 'incidence' plot object ('p') and add
## lines from an 'projections' object ('x'), as returned by projections::project

#' @export
#' @rdname plot.projections
#' @param p A previous incidence plot to which projections should be added.
add_projections <- function(p, x, y = c(0.01, 0.05, 0.1, 0.5),
                            palette = quantile_pal) {

  if (!inherits(x, "projections")) {
    stop("x must be a 'projections' object;",
         "\nsee ?projections::project")
  }
  y <- sort(unique(c(y, 1-y)))
  y <- y[y > 0 & y < 1]

  stats <- t(apply(x, 1, stats::quantile, y))
  dates <- attr(x, "dates")
  quantiles <- rep(colnames(stats), each = nrow(stats))
  quantiles <- factor(quantiles, levels = unique(quantiles))
  df <- cbind.data.frame(dates = rep(dates, ncol(stats)),
                         quantile = quantiles,
                         value = as.vector(stats),
                         stringsAsFactors = FALSE)

  colors <- color_quantiles(df$quantile, palette)

  p <- suppressMessages(
    p +
      ggplot2::geom_line(
        data = df,
        ggplot2::aes_string(x = "dates", y = "value", color = "quantile")) +
      ggplot2::scale_color_manual(values = colors)
  )

  p
}

