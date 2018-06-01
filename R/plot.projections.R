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
#' @param quantiles A vector of quantiles to plot, automatically completed to be
#'   symmetric around the median.
#'
#' @param palette A color palette to be used for plotting the quantile lines;
#'   defaults to \code{quantile_pal}.
#'
#' @param ribbon A logical indicating if a ribbon should be drawn; defaults to
#'   \code{TRUE}.
#'
#' @param ribbon_color Any valid color, used for the ribbon.
#'
#' @param ribbon_alpha A number used to control the transparency of the
#'   ribbon, from 0 (full transparency) to 1 (full opacity); defaults to 0.3.
#'
#' @param ribbon_quantiles A vector of 2 quantiles to be used to determine the
#'   limits of the ribbon; if NULL (default); uses the most extreme quantiles if
#'   available; if quantiles are not provided, the daily range will be used.
#'
#' @param boxplots A logical indicating if boxplots should be drawn.
#'
#' @param linetype An integer indicating the type of line used for plotting the
#'   quantiles; defaults to 1 for a plain line.
#'
#' @param linesize An integer indicating the size of line used for plotting the
#'   quantiles; defaults to 0.5.
#'
#' @param boxplots_color Any valid color, used for the boxplot.
#'
#' @param boxplots_alpha A number used to control the transparency of the
#'   boxplots, from 0 (full transparency) to 1 (full opacity); defaults to 0.8.
#'
#' @param quantiles_alpha A number used to control the transparency of the
#'   quantile lines, from 0 (full transparency) to 1 (full opacity); defaults to
#'   1.
#'
#' @param outliers A logical indicating if outliers should be displayed
#'   alongside the boxplots; defaults to \code{TRUE}.
#'
#' @param ... Further arguments to be passed to \code{add_projections}.
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
#' ## plotting projections: different options
#' plot(proj)
#' plot(proj, quantiles = c(.025, .5)) # 95% CI
#' plot(proj, ribbon_color = "red", quantiles = FALSE) # range
#' plot(proj, ribbon_color = "red", quantiles = FALSE,
#'      ribbon_quantiles = c(.025, .5))
#' plot(proj, boxplots = TRUE, quantiles = FALSE, ribbon = FALSE)
#' plot(proj, boxplots = TRUE, quantiles = FALSE, outliers = FALSE)
#' plot(proj, linetype = 3)
#'
#' ## adding them to incidence plot
#' plot(i) %>% add_projections(proj)
#' plot(i[1:160]) %>% add_projections(proj)
#' plot(i[1:160]) %>% add_projections(proj, boxplots = FALSE)
#' plot(i[1:160]) %>%
#'   add_projections(proj, boxplots_alpha = .3, boxplots_color = "red")
#'
#' ## same, with customised quantiles and colors
#' quantiles <- c(.001, .01, 0.05, .1, .2, .3, .4, .5)
#' pal <- colorRampPalette(c("#b3c6ff", "#00e64d", "#cc0066"))
#' plot(i[1:200]) %>%
#'   add_projections(proj, quantiles, palette = pal)
#'
#' }
#'

plot.projections <- function(x, ...) {
  empty <- ggplot2::ggplot()
  out <- add_projections(empty, x, ...)
  ylab <- ifelse(isTRUE(attr(x, "cumulative")),
                 "Predicted cumulative incidence",
                 "Predicted incidence")
  out <- out + ggplot2::labs(x = "", y = ylab)
  out
}





## This function will take an existing 'incidence' plot object ('p') and add
## lines from an 'projections' object ('x'), as returned by projections::project

#' @export
#' @rdname plot.projections
#' @param p A previous incidence plot to which projections should be added.
add_projections <- function(p, x, quantiles = c(0.01, 0.05, 0.1, 0.5),
                            ribbon = TRUE, boxplots = FALSE,
                            palette = quantile_pal,
                            quantiles_alpha = 1,
                            linetype = 1, linesize = 0.5,
                            ribbon_quantiles = NULL,
                            ribbon_color = NULL, ribbon_alpha = 0.3,
                            boxplots_color = "#47476b",
                            boxplots_alpha = 0.8,
                            outliers = TRUE) {

  if (!inherits(x, "projections")) {
    stop("x must be a 'projections' object;",
         "\nsee ?projections::project")
  }

  ## Strategy: we start off the provided plot, which may well be empty
  ## (i.e. output of ggplot2::ggplot()), and add layers depending on what the
  ## user wants. Currently available layers include:

  ## - quantiles
  ## - boxplots
  ## - ribbon

  out <- p
  dates <- attr(x, "dates")

  if (!is.null(quantiles) && !isFALSE(quantiles) && !is.na(quantiles)) {
    quantiles <- sort(unique(c(quantiles, 1 - quantiles)))
    quantiles <- quantiles[quantiles >= 0 & quantiles <= 1]
  }


  ## This is the part handling the ribbon

  if (isTRUE(ribbon)) {
    ## find the ymin and ymax for ribbon
    if (is.null(ribbon_quantiles)) {
      if (is.null(quantiles) || isFALSE(quantiles) || is.na(quantiles)) {
        ribbon_quantiles <- c(0, 1)
      } else {
        ribbon_quantiles <- range(quantiles)
      }
    }
    stats <- t(apply(x, 1, stats::quantile, ribbon_quantiles))
    df <- cbind.data.frame(dates, stats)
    names(df) <- c("dates", "ymin", "ymax")

    ## find colors; use the quantile's by default
    if (is.null(ribbon_color)) {
      ribbon_color <- color_quantiles(ribbon_quantiles, palette)[1]
    }
    ribbon_color <- transp(ribbon_color, ribbon_alpha)

    out <- out +
      ggplot2::geom_ribbon(
        data = df,
        ggplot2::aes_string(x = "dates", ymin = "ymin", ymax = "ymax"),
        fill = ribbon_color)
  }


  ## This is the part handling the boxplots

  if (isTRUE(boxplots)) {
    df <- as.data.frame(x, long = TRUE)
    out <- suppressMessages(
      out +
        ggplot2::geom_boxplot(
          data = df,
          ggplot2::aes_string(x = "date", y = "incidence", group = "date"),
          color = transp(boxplots_color, boxplots_alpha),
          outlier.size = 0.5,
          outlier.color = ifelse(outliers, boxplots_color, "transparent")
        )
      )
  }


  ## This is the part handling the quantile lines

  if (isFALSE(quantiles)) {
    quantiles <- NULL
  }
  if (!is.null(quantiles)) {
    stats <- t(apply(x, 1, stats::quantile, quantiles))
    quantiles <- rep(colnames(stats), each = nrow(stats))
    quantiles <- factor(quantiles, levels = unique(quantiles))
    df <- cbind.data.frame(dates = rep(dates, ncol(stats)),
                           quantile = quantiles,
                           value = as.vector(stats),
                           stringsAsFactors = FALSE)

    colors <- color_quantiles(df$quantile, palette)
    colors <- transp(colors, quantiles_alpha)

    out <- suppressMessages(
      out +
        ggplot2::geom_line(
          data = df,
          ggplot2::aes_string(x = "dates", y = "value", color = "quantile"),
          linetype = linetype,
          size = linesize
        ) +
        ggplot2::scale_color_manual(values = colors)
    )
  }

  out
}

