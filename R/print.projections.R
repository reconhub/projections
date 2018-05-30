#' Print method for projections objects
#'
#' This method prints the content of \code{projections} objects.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x A \code{projections} object.
#'
#' @param ... further parameters to be passed to other methods (currently not
#' used)
#'
print.projections <- function(x, ...){
  cat("\n/// Incidence projections //\n")
  cat("\n  // class:", paste(class(x), collapse=", "))
  cat("\n  //", format(nrow(x), big.mark=","),
      "dates (rows);",
      format(ncol(x), big.mark=","),
      "simulations (columns)\n")

  cat("\n // first rows/columns:\n")
  p <- min(6, ncol(x))
  n <- min(4, nrow(x))
  print(as.matrix(x)[1:n, 1:p, drop = FALSE])
  if (n < nrow(x)) replicate(3, cat(" .\n"))

  cat("\n // dates:\n")
  print(attr(x, "dates"))
  if (isTRUE(attr(x, "cumulative"))) {
    cat("\n // cumulative projections")
  }
  cat("\n")
}
