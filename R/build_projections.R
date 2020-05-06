#' @importFrom methods is
NULL

new_projections <- function(x, dates, cumulative, ..., class = character()) {
  
  stopifnot(is.array(x) || is.matrix(x))
  stopifnot(is(dates, "Date") || is.numeric(dates))
  stopifnot(is.logical(cumulative))
  
  if (length(dates) != nrow(x)) {
    stop(sprintf("Number of dates (%d) does not match number of rows (%d)",
                 length(dates), nrow(x))
    )
  }
  
  x <- as.matrix(x)
  rownames(x) <- as.character(dates)
  
  structure(x,
            ...,
            dates = dates,
            cumulative = cumulative,
            class = c(class, "projections", class(x)))
}



#' Constructor for projections objects
#'
#' This function builds a valid \code{projections} object from some input
#' simulations and dates.
#'
#' @author Thibaut Jombart \email{thibautjombart@gmail.com}
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
#' @param order_dates A logical indicating whether the dates should be ordered,
#'   from the oldest to the most recent one; `TRUE` by default.
#'
#'
#' @export
#'
#'
#' @seealso the \code{\link{project}} function to generate the 'projections'
#'   objects.
#'
#'
build_projections <- function(x, dates = NULL, cumulative = FALSE,
                              order_dates = TRUE) {
  x <- as.matrix(x) # todo - this allows dataframes to be used.  This is
                    #        tested for but not documented.
  
  if (is.null(dates)) {
    dates <- seq_len(nrow(x)) - 1L
  }
  if (length(dates) != nrow(x)) {
    stop(
      sprintf(
        "Number of dates (%d) does not match number of rows (%d)",
        length(dates), nrow(x))
      )
  }

  ## reorder dates
  if (order_dates) {
    idx <- order(dates)
    dates <- sort(dates)
    x <- x[idx, , drop = FALSE]
  }
  
  new_projections(x, dates, cumulative)
}
