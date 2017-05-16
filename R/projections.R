#' Project future incidence
#'
#' This function simulates future incidence based on past incidence data, a
#' selection of plausible reproduction numbers (R), and the distribution of the
#' serial interval (time from primary onset to secondary onset).
#'
#' @export
#'
#' @param x An \code{incidence} object containing daily incidence; other time
#' intervals will trigger an error.
#'
#' @param R A vector of numbers representing plausible reproduction numbers; for
#' instance, these can be samples from a posterior distribution using the
#' \code{EpiEstim} package.
#'
#' @param si A function computing the serial interval. We strongly recommend
#' using the RECON package \code{distcrete} to obtain such distribution (see
#' example).
#'
#' @param n_sim The number of epicurves to simulate. Defaults to 100.
#'
#' @param n_days The number of days to run simulations for. Defaults to 14.
#'
#'

project <- function(x, R, si, n_sim = 100, n_days = 7) {

  ## Various checks on inputs
  
  if (!inherits(x, "incidence")) {
    msg <- "x is not an incidence object"
    stop(msg)
  }
  
  if (as.integer(x$interval) != 1L) {
    msg <- sprintf("daily incidence needed (current interval is %d days)",
                   x$interval)
    stop(msg)
  }
  
  if ( (names(si_dist)=='shape') != 1) {
    msg <- sprintf("si_dist does not contain a shape)",
                   si_dist)
    stop(msg)
  }
  
  if ( (names(si_dist)=='scale') != 1) {
    msg <- sprintf("si_dist does not contain a scale)",
                   si_dist)
    stop(msg)
  }
  
  ## number of days with observations
  t_prior <- length(x)
  
  ## serial interval and its reversed
  si <- distcrete("gamma", interval = 1L, shape = si_dist$shape, scale = si_dist$scale)$d
  ws <- rev(si(0:(n_days+t_prior-1)))
  
  ## initial conditions
  I0 <- x%*%matrix(1,1,n_sim)
  
  ## projection
  I_proj <- rbind(I0,matrix(0,n_days,n_sim))
  for (i in (t_prior+1):(t_prior+n_days)){
    lambda <- tail(ws,i) %*% I_new[1:i,]
    I_proj[i,] <- rpois(n_sim,R*lambda)
  }
  # store simulations
  I_proj <- I_proj[(t_prior+1):(t_prior+n_days),]
  
  
  return(I_proj)

}

