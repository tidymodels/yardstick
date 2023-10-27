#' @return
#'
#' This function outputs a yardstick _fairness metric_ function. Given a
#' grouping variable `by`, `<%=fn %>()` will return a yardstick metric
#' function that is associated with the data-variable grouping `by` and a
#' post-processor. The outputted function will first generate a set
#' of <%=internal_fn %> metric values by group before summarizing across
#' groups using the post-processing function.
#'
#' The outputted function only has a data frame method and is intended to
#' be used as part of a metric set.
