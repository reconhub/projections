#' Compute cumulative projections
#'
#' \code{cumulate} is an S3 generic to compute cumulative numbers defined in the
#' package \code{incidence}. The method for \code{projections} objects turns
#' predicted incidences into cumulative incidences over time.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @seealso The \code{\link{project}} function to generate the
#'   \code{projections} objects.
#'
#' @param x A \code{projections} object.
#'
#' @examples
#'
#' if (require(distcrete) &&
#'     require(incidence)) {
#'
#'   ## simulate basic epicurve
#'   dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
#'   i <- incidence(dat)
#'
#'
#'   ## example with a function for SI
#'   si <- distcrete("gamma", interval = 1L,
#'                   shape = 1.5,
#'                   scale = 2, w = 0)
#'   set.seed(1)
#'   pred_1 <- project(i, runif(100, 0.8, 1.9), si, n_days = 30)
#'   plot_1 <- plot(pred_1)
#'
#'   ## cumulative predictions
#'   pred_1_cum <- cumulate(pred_1)
#'   pred_1_cum
#'   plot(pred_1_cum)
#' }
#'
#'
#' @importFrom incidence cumulate
#' @export
#' @aliases cumulate.projections
#'
cumulate.projections <- function(x) {
  if (isTRUE(attr(x, "cumulative"))) {
    stop("x is already a cumulative incidence")
  }
  out <- x
  out[] <- apply(out[], 2, cumsum)
  attr(out, "cumulative") <- TRUE
  out
}
