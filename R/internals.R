
## Internal functions (not exported)

assert_reporting <- function(x) {
  if (!is.numeric(x)) stop("reporting is not numeric")
  if (!is.finite(x)) stop("reporting is not a finite value")
  if (x <= 0) stop("reporting <= 0")
  if (x > 1) stop("reporting > 1")
}




## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
