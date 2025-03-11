#' @return
#'
#' A `tibble` with columns `.metric`, `.estimator`, and `.estimate`.
#'
#' For an ungrouped data frame, the result has one row of values. For a grouped data frame,
#' the number of rows returned is the same as the number of groups.
#'
#' For `<%=fn %>_vec()`, a `numeric` vector same length as the input argument
#' `eval_time`. (or `NA`).
