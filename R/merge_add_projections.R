##' Add cases from different projections objects
##'
##' This function adds cases from a list of `projections` objects with the same
##' number of simulations, but potentially different dates.
##'
##' 


#' @param x a `list` of `projections` objects to be merged
#'

merge_add_projections <- function(x) {

  ## add input validators here
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

  ## 4. replace all NAs by 0

  ## 5. add data from the different data.frame

  ## 6. build a new `projections` object


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
  list_matrices <- lapply(list_df_complete,
                          function(e) as.matrix(e[, -1], drop = FALSE))
  for (i in seq_along(list_matrices)) {
    list_matrices[[i]][is.na(list_matrices[[i]])] <- 0
  }
  
  ## step 5
  out_matrix <- Reduce("+", list_matrices)
  
  out <- build_projections(out_matrix, all_dates)
  out
}
