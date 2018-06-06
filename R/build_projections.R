#' Constructor for projections objects
#'
#' This function builds a valid \code{projections} object from some input
#' simulations and dates.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#'
#' @export
#'
#' @param x A \code{matrix} of simulated incidence stored as integers, where
#'   rows correspond to dates and columns to simulations.
#'
#' @param dates A vector of dates containing one value per row in \code{x};
#'   acceptable formats are: \code{integer}, \code{Date}, and \code{POSIXct}; if
#'   NULL, the time steps will be counted, with the first dates corresponding to
#'   0.
#'
#' @param cumulative A logical indicating if data represent cumulative
#'   incidence; defaults to \code{FALSE}.
#'
#'
#' @export
#'
#'
#' @seealso the \code{\link{project}} function to generate the 'projections'
#'   objects.
#'
#'

build_projections <- function(x, dates = NULL, cumulative = FALSE) {
  out <- as.matrix(x)
  if (is.null(dates)) {
    dates <- seq_len(nrow(x)) - 1L
  }
  if (length(dates) != nrow(out)) {
    stop(
      sprintf(
        "Number of dates (%d) does not match number of rows (%d)",
        length(dates), nrow(out))
      )
  }
  attr(out, "dates") <- dates
  rownames(out) <- as.character(dates)
  attr(out, "cumulative") <- cumulative
  class(out) <- c("projections", "matrix")
  out
}
