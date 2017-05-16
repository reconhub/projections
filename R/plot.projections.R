#' Plot projections objects
#'
#' This method is designed for plotting \code{projections} objects, output by
#' the function \code{\link{project}}.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#'
#' @param x A \code{projections} object.
#' 
plot.projections <- function(x, y = NULL, ...) {
    df <- as.data.frame(x, long = TRUE)

    
}
