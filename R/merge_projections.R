#' Merge a list of projections objects
#'
#' This function merges `projections` objects, binding them by columns, making
#' sure that they all use the same dates, adding rows of '0' where needed.
#'
#' @author Thibaut Jombart
#' 
#' @param x A `list` of `projections` objects to be merged.
#' 
#' @examples
#' 


merge_projections <- function(x) {

  ## check that inputs are all okay
  if (!is.list(x)) {
    msg <- sprintf("x is not a `list` but a %s",
                   class(x)[1])
    stop(msg)
  }
  
  is_projections <- vapply(x,
                           function(e) inherits(e, "projections"),
                           logical(1))
  if (!all(is_projections)) {
    msg <- "some input objects are not `projections` objects"
    stop(msg)
  }

  ## note: Reduce(function(...) merge(..., all = TRUE), proj) would work here
  ## but take a loooot of time; `dplyr::full_join` is worse; we do the merge
  ## manually instead

  ## Strategy is:

  ## 1. convert all objects to `data.frame`

  ## 2. finding all dates and making a vector of unique dates; we also store
  ## it as a data.frame for merging

  ## 3. merge this date data.frame to all other data.frames, so they all have
  ## the same temporal references

  ## 4. cbind all obtained data.frames, excluding the dates column, using
  ## do.call for computer efficiency; replace all NAs by 0

  ## 5. build a new `projections` object


  ## step 1
  list_df <- lapply(x, as.data.frame)


  ## step 2
  all_dates <- Reduce(c, lapply(list_df, function(e) e$dates))
  all_dates <- sort(unique(all_dates)) # sorting is important!!
  all_dates_df <- data.frame(dates = all_dates)

  ## step 3
  list_df_complete <- lapply(list_df,
                             function(e) merge(all_dates_df, e, all = TRUE))

  ## step 4
  out_matrix <- do.call(cbind, lapply(list_df_complete, function(e) e[, -1]))
  out_matrix[is.na(out_matrix)] <- 0
  colnames(out_matrix) <- paste("sim", seq_len(ncol(out_matrix)), sep = "_")

  # step 5
  out <- build_projections(out_matrix, all_dates)
  out
}
