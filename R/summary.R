
#' Summary for projections objects
#'
#' This method summarises predicted epidemic trajectories contained in a
#' `projections` object by days, deriving the mean, standard deviation, and
#' user-specified quantiles for each day.
#'
#' @author Thibaut Jombart
#'
#' @export
#'
#' @aliases summary.projections
#'
#' @param object A `projections` object to summarise
#'
#' @param quantiles A `numeric` vector indicating which quantiles should be
#'   computed; ignored if `FALSE` or of length 0
#'
#' @param mean a `logical` indicating of the mean should be computed
#'
#' @param sd a `logical` indicating of the standard deviation should be computed
#' 
#' @param min a `logical` indicating of the minimum should be computed
#' 
#' @param max a `logical` indicating of the maximum should be computed
#'
#' @param ... only preesnt for compatibility with the generic
#'
#' @examples
#' if (require(incidence)) {
#'   i <- incidence::incidence(as.Date('2020-01-23'))
#'   si <- c(0.2, 0.5, 0.2, 0.1)
#'   R0 <- 2
#'
#'   p <- project(x = i,
#'                si = si,
#'                R = R0,
#'                n_sim = 2, 
#'                R_fix_within = TRUE,
#'                n_days = 10,
#'                model = "poisson"
#'                )
#'   summary(p)
#'
#' }

summary.projections <- function(object,
                                quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                mean = TRUE,
                                sd = TRUE,
                                min = TRUE,
                                max = TRUE,
                                ...) {

  ## This auxiliary function will calculate summaries for a single day
  f_summary <- function(x, mean, sd, min, max, quantiles) {
    x <- as.vector(x)
    out <- list()
    if (mean) {
      out$mean <- mean(x, na.rm = TRUE)
    }
    if (sd) {
      out$sd <- sd(x, na.rm = TRUE)
    }
    if (min) {
      out$min <- min(x, na.rm = TRUE)
    }
    if (max) {
      out$max <- max(x, na.rm = TRUE)
    }
    if (!isFALSE(quantiles) & length(quantiles)) {
      out$quantiles <- stats::quantile(x,
                                       quantiles,
                                       na.rm = TRUE)
    }
    do.call(c, out)
  }

  out <- apply(object, 1, f_summary,
               mean = mean,
               sd = sd,
               min = min,
               max = max,
               quantiles = quantiles
               )
  out <- cbind.data.frame(dates = get_dates(object),
                          as.data.frame(t(out)))
  rownames(out) <- NULL
  out
}

