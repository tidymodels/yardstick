#' @section Measuring Disparity:
#' By default, this function takes the difference in range of <%=internal_fn %>
#' `.estimate`s across groups. That is, the maximum pair-wise disparity between
#' groups is the return value of `<%=fn %>()`'s `.estimate`.
#'
#' For finer control of group treatment, construct a context-aware fairness
#' metric with the [fairness_metric()] function by passing a custom `.post`
#' function:
#'
#' ```
#' # the actual default `.post` is:
#' diff_range <- function(x, ...) {diff(range(x$.estimate))}
#'
#' <%=fn %>_2 <-
#'   fairness_metric(
#'     .fn = <%=internal_.fn %>,
#'     .name = "<%=fn %>_2",
#'     .post = diff_range
#'   )
#' ```
#'
#' In `.post()`, `x` is the `metric_set()` output with <%=internal_fn %> values
#' for each group, and `...` gives additional arguments (such as a grouping
#' level to refer to as the "baseline") to pass to the function outputted
#' by `<%=fn %>_2()` for context.
