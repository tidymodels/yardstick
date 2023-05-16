#' Create fairness metrics
#'
#' Fairness metrics quantify the disparity in value of a metric across a number
#' of groups. Fairness metrics with a value of zero indicate that the
#' underlying metric has parity across groups. yardstick defines
#' several common fairness metrics using this function, such as
#' [demographic_parity()], [equal_opportunity()], and [equalized_odds()].
#'
#' @param .fn A yardstick metric function or metric set.
#' @param .name The name of the metric to place in the `.metric` column
#' of the output.
#' @param .post A function to post-process the generated metric set results `x`.
#' In many cases, `~diff(range(x$.estimate))` or
#' `~r <- range(x$.estimate); r[1]/r[2]`.
#'
#' @section Relevant Group Level:
#' By default,
#'
#' Additional arguments can be passed to the function outputted by
#' the function that this function outputs. That is:
#'
#' ```
#' res_fairness <- fairness_metric(...)
#' res_by <- res_fairness(by)
#' res_by(..., additional_arguments_to_.post = TRUE)
#' ```
#'
#' For finer control of how groups in `by` are treated, use the
#' `.post` argument.
#'
#' @return A function with one argument, `by`, indicating the data-masked
#' variable giving the sensitive feature. See the documentation on the
#' return value of fairness metrics like [demographic_parity()],
#' [equal_opportunity()], or [equalized_odds()] to learn more about how the
#' output of this function can be used.
#'
#' @examples
#' data(hpc_cv)
#'
#' # `demographic_parity`, among other fairness metrics,
#' # is generated with `fairness_metric()`:
#' diff_range <- function(x, ...) {diff(range(x$.estimate))}
#' demographic_parity_ <-
#'   fairness_metric(
#'     .fn = detection_prevalence,
#'     .name = "demographic_parity",
#'     .post = diff_range
#'   )
#'
#' m_set <- metric_set(demographic_parity_(Resample))
#'
#' m_set(hpc_cv, truth = obs, estimate = pred)
#'
#' # the `post` argument can be used to accommodate a wide
#' # variety of parameterizations. to encode demographic
#' # parity as a ratio inside of a difference, for example:
#' ratio_range <- function(x, ...) {
#'   range <- range(x$.estimate)
#'   range[1] / range[2]
#' }
#'
#' demographic_parity_ratio <-
#'   fairness_metric(
#'     .fn = detection_prevalence,
#'     .name = "demographic_parity_ratio",
#'     .post = ratio_range
#'   )
#'
#' @export
fairness_metric <- function(.fn, .name, .post) {
  if (is_missing(.fn) || !inherits_any(.fn, c("metric", "metric_set"))) {
    abort("`.fn` must be a metric function or metric set.")
  }
  if (is_missing(.name) || !is_string(.name)) {
    abort("`.name` must be a string.")
  }
  if (is_missing(.post) || !is_function(.post)) {
    abort("`.post` must be a function.")
  }

  function(by) {
    by_str <- as_string(enexpr(by))
    res <-
      function(data, ...) {
        gp_vars <- dplyr::group_vars(data)

        res <- dplyr::group_by(data, {{by}}, .add = TRUE)
        res <- .fn(res, ...)

        if (length(gp_vars) > 0) {
          splits <- vec_split(res, res[gp_vars])
          .estimate <- vapply(splits$val, .post, numeric(1), ...)
        } else {
          .estimate <- .post(res, ...)
        }

        if (!is_bare_numeric(.estimate)) {
          abort(
            "`.post` must return a single numeric value.",
            call = call2("fairness_metric")
          )
        }

        if (length(gp_vars) > 0) {
          res <- dplyr::group_by(res, !!!dplyr::groups(data), .add = FALSE)
        }

        res <-
          dplyr::summarize(
            res,
            .metric = .name,
            !!".by" := by_str,
            .estimator = .estimator[1],
            .groups = "drop"
          )

        res$.estimate <- .estimate

        res
      }
    res <- new_class_metric(res, direction = "minimize")
    attr(res, "by") <- by_str
    res
  }
}

diff_range <- function(x, ...) {
  diff(range(x$.estimate))
}

max_positive_rate_diff <- function(x, ...) {
  metric_values <- vec_split(x, x$.metric)

  positive_rate_diff <- vapply(metric_values$val, diff_range, numeric(1), ...)

  max(positive_rate_diff)
}
