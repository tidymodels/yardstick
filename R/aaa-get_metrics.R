#' Get all metrics of a given type
#'
#' `get_metrics()` returns a [metric_set()] containing all yardstick metrics of
#' the specified type(s).
#'
#' @param type A character vector of metric types. Valid types are: `"class"`,
#'   `"prob"`, `"ordered_prob"`, `"numeric"`, `"dynamic_survival"`,
#'   `"integrated_survival"`, `"static_survival"`, `"linear_pred_survival"`,
#'   and `"quantile"`.
#'
#' More than 1 type can be selected but you are constrained by which metric
#' types [metric_set()] allows to be combined. This means that
#' `get_metrics(c("class", "prob"))`  will run without error, but
#' `get_metrics(c("class", "numeric"))` will return an error because you can't
#' combine `"class"` and `"numeric"` metrics.
#'
#' @return A [metric_set()] containing all metrics of the specified type(s).
#'
#' @seealso [metric_set()]
#'
#' @examples
#' get_metrics("numeric")
#'
#' get_metrics("class")
#'
#' # Get multiple types at once
#' get_metrics(c("class", "prob"))
#'
#' @export
get_metrics <- function(type) {
  yardns <- asNamespace("yardstick")
  fns <- lapply(names(yardns), get, envir = yardns)
  names(fns) <- names(yardns)

  metric_types <- grep("^new_.*_metric$", names(fns), value = TRUE)
  metric_types <- gsub("(new_|_metric)", "", metric_types)
  rlang::arg_match(type, metric_types, multiple = TRUE)

  type <- paste0(type, "_metric")
  metrics <- fns[vapply(fns, inherits, what = type, FUN.VALUE = logical(1))]

  metric_set(!!!metrics)
}
