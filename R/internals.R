
## Internal functions (not exported)

assert_reporting <- function(x) {
  if (!is.numeric(x)) stop("reporting is not numeric")
  if (!is.finite(x)) stop("reporting is not a finite value")
  if (x <= 0) stop("reporting <= 0")
  if (x > 1) stop("reporting > 1")
}
