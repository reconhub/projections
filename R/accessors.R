#' Access content projections objects
#'
#' These simple helper functions retrieve content from \code{projections}
#' objects. They currently include:
#'
#' \itemize{
#'  \item \code{get_dates}: get dates of the predictions.
#'
#' }
#' @name get_dates
#' @rdname projections_accessors
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#'
#' @param x A \code{projections} object.
#'
#' @param ... Further arguments passed to methods; currently not used.
#'
#' @examples
#'
#'
#' if (require(distcrete) && require(incidence)) { withAutoprint({
#'
#' ## prepare input: epicurve and serial interval
#' dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
#' i <- incidence(dat)
#' si <- distcrete("gamma", interval = 1L,
#'                  shape = 1.5,
#'                  scale = 2, w = 0)
#'
#'
#' ## make predictions
#' pred_1 <- project(i, 1.2, si, n_days = 30)
#' pred_1
#'
#'
#' ## retrieve content
#' get_dates(pred_1)
#' max(i$dates) # predictions start 1 day after last incidence
#'
#' })}
#' @aliases get_dates.projections
#' @aliases get_dates
#' @importFrom incidence get_dates
#' @export get_dates
#' @export
get_dates.projections <- function(x, ...) {
    attr(x, "dates")
}


