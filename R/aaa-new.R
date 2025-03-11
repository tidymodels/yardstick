# File is named with `aaa-` so that it is loaded before any other files. We need
# to call `new_*_metric()` internally from outside any function in the package,
# so this file has to be sourced first. It is a bit of a hack, but works.

# ------------------------------------------------------------------------------

#' Construct a new metric function
#'
#' @description
#' These functions provide convenient wrappers to create the three types of
#' metric functions in yardstick: numeric metrics, class metrics, and
#' class probability metrics. They add a metric-specific class to `fn` and
#' attach a `direction` attribute. These features are used by [metric_set()]
#' and by [tune](https://tune.tidymodels.org/) when model tuning.
#'
#' See [Custom performance
#' metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
#' information about creating custom metrics.
#'
#' @param fn A function. The metric function to attach a metric-specific class
#'   and `direction` attribute to.
#'
#' @param direction A string. One of:
#'   - `"maximize"`
#'   - `"minimize"`
#'   - `"zero"`
#'
#' @name new-metric
NULL

#' @rdname new-metric
#' @export
new_class_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "class_metric")
}

#' @rdname new-metric
#' @export
new_prob_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "prob_metric")
}

#' @rdname new-metric
#' @export
new_ordered_prob_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "ordered_prob_metric")
}

#' @rdname new-metric
#' @export
new_numeric_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "numeric_metric")
}

#' @rdname new-metric
#' @export
new_dynamic_survival_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "dynamic_survival_metric")
}

#' @rdname new-metric
#' @export
new_integrated_survival_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "integrated_survival_metric")
}

#' @rdname new-metric
#' @export
new_static_survival_metric <- function(fn, direction) {
  new_metric(fn, direction, class = "static_survival_metric")
}

#' @include import-standalone-types-check.R
new_metric <- function(fn, direction, class = NULL, call = caller_env()) {
  check_function(fn, call = call)

  direction <- arg_match(
    direction,
    values = c("maximize", "minimize", "zero"),
    error_call = call
  )

  class <- c(class, "metric", "function")

  structure(
    fn,
    direction = direction,
    class = class
  )
}

is_metric <- function(x) {
  inherits(x, "metric")
}

metric_direction <- function(x) {
  attr(x, "direction", exact = TRUE)
}
`metric_direction<-` <- function(x, value) {
  attr(x, "direction") <- value
  x
}

#' @noRd
#' @export
print.metric <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
format.metric <- function(x, ...) {
  first_class <- class(x)[[1]]
  metric_type <-
    switch(
      first_class,
      "prob_metric" = "probability metric",
      "class_metric" = "class metric",
      "ordered_prob_metric" = "ordered probability metric",
      "numeric_metric" = "numeric metric",
      "dynamic_survival_metric" = "dynamic survival metric",
      "static_survival_metric" = "static survival metric",
      "integrated_survival_metric" = "integrated survival metric",
      "metric"
    )

  metric_desc <- "direction: {.field {attr(x, 'direction')}}"

  by_attr <- attr(x, "by")
  if (!is.null(by_attr)) {
    metric_desc <-
      c(
        metric_desc,
        ", group-wise on: {.field {as.character(by_attr)}}"
      )
  }

  cli::cli_format_method(
    cli::cli_text(c("A {metric_type} | ", metric_desc))
  )
}
