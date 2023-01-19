#' @return
#'
#' A `tibble` with columns `.metric`, `.estimator`,
#' and `.estimate` and 1 row of values. `.estimate` will be a list column of
#' tibbles. Each containing 2 columns `.time` and `.estimate`, with the number
#' of rows corresponding to the input argument `.time`.
#'
#' For grouped data frames, the number of rows returned will be the same as
#' the number of groups.
#'
#' For `<%=fn %>_vec()`, a `numeric` vector same length as the input argument
#' `.time`. (or `NA`).
