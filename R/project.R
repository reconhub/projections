#' Project future incidence
#'
#' This function simulates future incidence based on past incidence data, a
#' selection of plausible reproduction numbers (R), and the distribution of the
#' serial interval (time from primary onset to secondary onset).
#'
#' @export
#'
#' @author Pierre Nouvellet and Thibaut Jombart
#'
#' @param x An \code{incidence} object containing daily incidence; other time
#' intervals will trigger an error.
#'
#' @param R A vector of numbers representing plausible reproduction numbers; for
#' instance, these can be samples from a posterior distribution using the
#' \code{EpiEstim} package.
#'
#' @param si A function computing the serial interval, or a \code{numeric}
#' vector providing its mass function. For functions, we strongly recommend
#' using the RECON package \code{distcrete} to obtain such distribution (see
#' example).
#'
#' @param n_sim The number of epicurves to simulate. Defaults to 100.
#'
#' @param n_days The number of days to run simulations for. Defaults to 14.
#'
#' @param R_fix_within A logical indicating if R should be fixed within
#'     simulations (but still varying across simulations). If \code{FALSE}, R is
#'     drawn for every simulation and every time step. Fixing values within
#'     simulations favours more extreme predictions (see details)
#'
#' @param model Distribution to be used for projections. Must be one of
#' "poisson" or "negbin" (negative binomial process). Defaults to poisson
#'
#' @param size size parameter of negative binomial distribition. Ignored if
#' model is poisson
#'
#' @details The decision to fix R values within simulations
#'     (\code{R_fix_within}) reflects two alternative views of the uncertainty
#'     associated with R. When drawing R values at random from the provided
#'     sample, (\code{R_fix_within} set to \code{FALSE}), it is assumed that R
#'     varies naturally, and can be treated as a random variable with a given
#'     distribution. When fixing values within simulations (\code{R_fix_within}
#'     set to \code{TRUE}), R is treated as a fixed parameter, and the
#'     uncertainty is merely a consequence of the estimation of R. In other
#'     words, the first view is rather Bayesian, while the second is more
#'     frequentist.
#'
#'
#' @examples
#'
#' ## example using simulated Ebola outbreak
#' if (require(outbreaks) &&
#'     require(distcrete) &&
#'     require(incidence) &&
#'     require(magrittr)) {
#'
#' si <- distcrete("gamma", interval = 1L,
#'                  shape = 0.37,
#'                  scale = 41.4, w = 0)
#'
#' i <- incidence(ebola_sim$linelist$date_of_onset)
#' plot(i)
#'
#'
#' ## projections after the first 100 days, over 60 days, fixed R to 2.1
#'
#' set.seed(1)
#' proj_1 <- project(x = i[1:100], R = 2.1, si = si, n_days = 60)
#' plot(proj_1)
#'
#' ## add projections to incidence plot
#' plot(i[1:160]) %>% add_projections(proj_1)
#'
#'
#' ## projections after the first 100 days, over 60 days, varying R
#'
#' set.seed(1)
#' R <- rnorm(100, 1.8, 0.2)
#' hist(R, col = "grey", border = "white", main = "Distribution of R")
#' proj_2 <- project(x = i[1:100], R = R, si = si, n_days = 60)
#'
#' ## add projections to incidence plot
#' plot(i[1:160]) %>% add_projections(proj_2)
#'
#'
#' ## same with R constant per simulation (more variability)
#'
#' set.seed(1)
#' proj_3 <- project(x = i[1:100], R = R, si = si, n_days = 60,
#'                   R_fix_within = TRUE)
#'
#' ## add projections to incidence plot
#' plot(i[1:160]) %>% add_projections(proj_3)
#'
#' }
#'

project <- function(x, R, si, n_sim = 100, n_days = 7,
                    R_fix_within = FALSE,
                    model = c("poisson", "negbin"),
                    size = 0.03) {

  ## Various checks on inputs

  model <- match.arg(model)

  if (!inherits(x, "incidence")) {
    msg <- "x is not an incidence object"
    stop(msg)
  }

  if (as.integer(x$interval) != 1L) {
    msg <- sprintf(
      "daily incidence needed, but interval is %d days",
      x$interval
    )
    stop(msg)
  }


  if (ncol(x$counts) > 1L) {
    msg <- sprintf("cannot use multiple groups in incidence object")
    stop(msg)
  }


  ## useful variables
  n_dates_x <- nrow(x$counts)
  t_max <- n_days + n_dates_x - 1

  if (inherits(si, "distcrete")) {
    if (as.integer(si$interval) != 1L) {
      msg <- sprintf(
        "interval used in si is not 1 day, but %d)",
        si$interval
      )
      stop(msg)
    }

    ws <- rev(si$d(0:t_max))
  } else {
    si <- si / sum(si)
    si <- c(si, rep(0, t_max))
    ws <- rev(si)
  }




  ## Computation of projections: we use the basic branching process with a
  ## Poisson likelihood, identical to the one used for estimating R0 in EpiEstim
  ## and earlyR. The force of infection on a given day is the sum of individual
  ## forces of infection. The invididual force of infection is computed as the
  ## R0 multiplied by the pmf of the serial interval for the corresponding day:

  ## lambda_{i,t} = R0 w(t - t_i)

  ## where 'w' is the PMF of the serial interval and 't_i' is the date of
  ## onset of case 'i'.


  ## initial conditions
  I0 <- matrix(x$counts, nrow = n_dates_x, ncol = n_sim)

  ## projection
  out <- rbind(I0, matrix(0, n_days, n_sim))
  t_start <- n_dates_x + 1
  t_stop <- t_max + 1
  t_sim <- seq(
    from = t_start,
    to = t_stop,
    by = 1L
  )


  ## On the drawing of R values: either these are constant within simulations,
  ## so drawn once for all simulations, or they need drawing at every time step
  ## for every simulations.


  ## On the handling of reporting: reporting first affects the force of
  ## infection lambda, with the underlying assumption that the true epicurve
  ## multiplied by the (constant) reporting results in the observed one. Then,
  ## the true incidence (true_I) is determined using this lambda (Poisson
  ## process) and we apply effects sampling to this true incidence to get the
  ## projected one, using a Binomial sampling.

  if (R_fix_within) {
    R <- sample_(R, n_sim, replace = TRUE)
  }

  for (i in t_sim) {
    if (!R_fix_within) {
      R <- sample_(R, n_sim, replace = TRUE)
    }
    lambda <- utils::tail(ws, i) %*% out[1:i, ]
    ## lambda <- lambda / reporting
    if (model == "poisson") {
      out[i, ] <- stats::rpois(n_sim, R * lambda)
    } else {
      ## If mu = 0, then it doesn't matter what the size value is,
      ## rnbinom will output 0s (mu = 0 => p =1).
      ## mu will be 0 if lambda is 0. But that will make size 0 which
      ##  will make rnbinom spit NAs. Workaround is: if lambda is 0
      ## set size to a non-trivial value.
      size_adj <- lambda * size
      idx <- which(lambda == 0)
      size_adj[idx] <- 1
      out[i, ] <- stats::rnbinom(n_sim, size = size_adj, mu = R * lambda)
    }
    ## out[i,] <- stats::rbinom(ncol(out), true_I, prob = reporting)
  }


  ## shape output: 'projections' objects are basically matrices of predicted
  ## incidence, with dates in rows and simulations in columns. Dates are
  ## stored as attributes of the object, in a format similar to that of the
  ## original dates in the 'incidence' object. We also store the original
  ## 'incidence' object in the attributes.

  out <- out[(n_dates_x + 1):(n_dates_x + n_days), ]

  dates <- utils::tail(x$dates, 1) + 1:nrow(out)

  build_projections(out, dates, FALSE)
}
