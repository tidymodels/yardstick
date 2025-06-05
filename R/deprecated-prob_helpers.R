# nocov start

# `...` -> estimate matrix / vector helper -------------------------------------

#' Developer helpers
#'
#' Helpers to be used alongside [check_metric], [yardstick_remove_missing] and
#' [metric summarizers][class_metric_summarizer()] when creating new metrics.
#' See [Custom performance
#' metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
#' information.
#'
#' @section Dots -> Estimate:
#' `r lifecycle::badge("deprecated")`
#'
#' `dots_to_estimate()` is useful with class probability metrics that take
#' `...` rather than `estimate` as an argument. It constructs either a single
#' name if 1 input is provided to `...` or it constructs a quosure where the
#' expression constructs a matrix of as many columns as are provided to `...`.
#' These are eventually evaluated in the `summarise()` call in
#' [metric-summarizers] and evaluate to either a vector or a matrix for
#' further use in the underlying vector functions.
#'
#'
#' @name developer-helpers
#'
#' @aliases dots_to_estimate
#'
#' @export
#'
#' @inheritParams roc_auc
dots_to_estimate <- function(data, ...) {
  lifecycle::deprecate_soft(
    when = "1.2.0",
    what = "dots_to_estimate()",
    details = I(
      paste(
        "No longer needed with",
        "`prob_metric_summarizer()`, or `curve_metric_summarizer()`."
      )
    )
  )

  # Capture dots
  dot_vars <- with_handlers(
    tidyselect::vars_select(names(data), !!!enquos(...)),
    tidyselect_empty_dots = function(cnd) {
      abort("No valid variables provided to `...`.")
    }
  )

  # estimate is a matrix of the selected columns if >1 selected
  dot_nms <- lapply(dot_vars, as.name)

  if (length(dot_nms) > 1) {
    estimate <- quo(
      matrix(
        data = c(!!!dot_nms),
        ncol = !!length(dot_nms),
        dimnames = list(NULL, !!dot_vars)
      )
    )
  } else {
    estimate <- dot_nms[[1]]
  }

  estimate
}

# nocov end
