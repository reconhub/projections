
## Internal functions (not exported)

assert_reporting <- function(x) {
  if (!is.numeric(x)) stop("reporting is not numeric")
  if (!all(is.finite(x))) stop("reporting is not a finite value")
  if (any(x <= 0)) stop("reporting <= 0")
  if (any(x > 1)) stop("reporting > 1")
}


assert_R <- function(x) {
  if (is.list(x)) {
    x <- unlist(x)
  }
  if (!is.numeric(x)) stop("R is not numeric")
  if (!all(is.finite(x))) stop("R is not a finite value")
  if (any(x < 0)) stop(sprintf("R < 0 (value: %.2f)", x[x<0]))
}



## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}





## Define colors for quantiles
quantile_pal <- grDevices::colorRampPalette(
  c("#b3c6ff", "#d147a3", "#993366"), bias = 2)

color_quantiles <- function(x, palette = quantile_pal) {
  labels <- as.character(unique(x))
  dist_from_median <- 50 - abs(50-as.numeric(sub("%", "", labels)))
  out <- palette(51)[dist_from_median + 1]
  names(out) <- labels
  out
}




## Function making colors transparent

transp <- function(col, alpha = .5){
    res <- apply(grDevices::col2rgb(col), 2,
                 function(c)
                 grDevices::rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
    return(res)
}



## Implement isTRUE and isFALSE to avoid dep on R 3.5.0

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}
