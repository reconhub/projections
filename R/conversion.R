#' Conversion of projections objects
#'
#' These functions convert \code{projections} objects into other classes.
#'
#' @rdname conversions
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @importFrom stats as.ts
#'
#' @export
#'
#' @param x An \code{projections} object, or an object to be converted as
#'   \code{projections} (see details).
#'
#' @param ... Further arguments passed to other functions (no used).
#'
#' @param long A logical indicating if the output data.frame should be 'long',
#' i.e. where a single column containing 'groups' is added in case of data
#' computed on several groups.
#'
#'
#' @export
#'
#'
#' @seealso the \code{\link{project}} function to generate the 'projections' objects.
#'
#'


as.matrix.projections <- function(x, ...) {
  out <- x
  class(out) <- oldClass(out)[-1] # first class will be projections 
  attr(out, "dates")  <- NULL
  attr(out, "cumulative")  <- NULL
  rownames(out) <- as.character(get_dates(x))

  out
}




#' @export
#' @rdname conversions

as.data.frame.projections <- function(x, ..., long = FALSE){
  if (!long) {
    colnames(x) <- paste("sim", seq_len(ncol(x)), sep = "_")
    out <- cbind.data.frame(dates = attr(x, "dates"), as.matrix(x))
      } else {
        out <- data.frame(date = rep(attr(x, "dates"), ncol(x)),
                          incidence = as.vector(x),
                          sim = rep(seq_len(ncol(x)), each = nrow(x)))
      }

  row.names(out) <- NULL

  out
}
