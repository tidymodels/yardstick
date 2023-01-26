#' @return
#'
#' A `tibble` with columns `.metric`, `.estimator`,
#' and `.estimate` and 1 row of values.
#'
#' For grouped data frames, the number of rows returned will be the same as
#' the number of groups.
#'
#' For `<%=fn %>_vec()`, a `numeric` vector same length as the input argument
#' `.time`. (or `NA`).
