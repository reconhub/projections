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
#' @param si A function computing the serial interval, or a \code{numeric}
#' vector providing its mass function. For functions, we strongly recommend
#' using the RECON package \code{distcrete} to obtain such distribution (see
#' example).
#'
#' @param n_sim The number of epicurves to simulate. Defaults to 100.
#'
#' @param n_days The number of days to run simulations for. Defaults to 14.
#'
#' @examples
#'
#' if (require(distcrete) && require(incidence)) {
#'
#' ## simulate basic epicurve
#' dat <- c(0, 2, 2, 3, 3, 5, 5, 5, 6, 6, 6, 6)
#' x <- incidence(dat)
#' plot(x)
#'
#' 
#' ## example with a function for SI
#' 
#' si <- distcrete("gamma", interval = 1L,
#'                  shape = 5,
#'                  scale = 5)
#' barplot(si$d(0:100), main = "Serial Interval")
#'
#'
#' pred_1 <- project(x, 1.2, si, n_days = 20)
#' pred_1
#' my_col <- rgb(.1, .1, .8, .4)
#' matplot(pred1, type = "l", lty = 1, col = my_col)
#'
#'
#' ## example with empirical serial interval
#' si <- c(0, 1, 2, 1, 0.5)
#' pred_2 <- project(x, 1.2, si, n_days = 30)
#' pred_2
#' matplot(pred2, type = "l", lty = 1, col = my_col)
#' 
#' 
#' }
#' 

project <- function(x, R, si, n_sim = 100, n_days = 7) {

    ## Various checks on inputs
    
    if (!inherits(x, "incidence")) {
        msg <- "x is not an incidence object"
        stop(msg)
    }
    
    if (as.integer(x$interval) != 1L) {
        msg <- sprintf("daily incidence needed, but %d days",
                       x$interval)
        stop(msg)
    }

    
    if (ncol(x$counts) > 1L) {
        msg <- sprintf("cannot use multiple groups in incidence object",
                       si$interval)
        stop(msg)
    }


    ## useful variables
    n_dates_x <- nrow(x$counts)
    t_max <- n_days + n_dates_x - 1
  
    if (inherits(si, "distcrete")) {
        if (as.integer(si$interval) != 1L) {
            msg <- sprintf("interval used in si is not one day, but %d days)",
                           si$interval)
            stop(msg)
        }

        ws <- rev(si$d(0:t_max))
    } else {
        si <- si / sum(si)
        si <- c(si, rep(0, t_max ))
        ws <- rev(si)
    }
    


    
    ## Computation of projections: this is how we do bla bla
    ## TODO: complete plain explanation of how this works
    
    
    ## initial conditions
    I0 <- matrix(x$counts, nrow = n_dates_x, ncol = n_sim)
    
    ## projection
    out <- rbind(I0, matrix(0, n_days, n_sim))
    t_start <- n_dates_x + 1
    t_stop <- t_max + 1
    t_sim <- seq(from = t_start,
                 to = t_stop,
                 by = 1L)

    ## we get one value of R per simulation
    R <- sample(R, n_sim, replace = TRUE)
    
    for (i in t_sim){
        lambda <- utils::tail(ws, i) %*% out[1:i, ]
        out[i,] <- rpois(n_sim, R*lambda)
    }
    
    ## store simulations
    out <- out[(n_dates_x+1):(n_dates_x + n_days),]
    
    class(out) <- "projections"
    return(out)
}

