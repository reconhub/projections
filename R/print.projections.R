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
print.projections <- function(x, short = TRUE, ...){
    cat("\n/// Incidence projections //\n")
    cat("\n  // class:", paste(class(x), collapse=", "))
    cat("\n  //", format(nrow(x), big.mark=","),
        "dates;",
        format(ncol(x), big.mark=","),
        "simulations\n")

    cat("\n // dates:\n")
    print(attr(x, "dates"))
    cat("\n")
}
