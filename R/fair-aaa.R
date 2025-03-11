#' Create groupwise metrics
#'
#' Groupwise metrics quantify the disparity in value of a metric across a
#' number of groups. Groupwise metrics with a value of zero indicate that the
#' underlying metric is equal across groups. yardstick defines
#' several common fairness metrics using this function, such as
#' [demographic_parity()], [equal_opportunity()], and [equalized_odds()].
#'
#' Note that _all_ yardstick metrics are group-aware in that, when passed
#' grouped data, they will return metric values calculated for each group.
#' When passed grouped data, groupwise metrics also return metric values
#' for each group, but those metric values are calculated by first additionally
#' grouping by the variable passed to `by` and then summarizing the per-group
#' metric estimates across groups using the function passed as the
#' `aggregate` argument. Learn more about grouping behavior in yardstick using
#' `vignette("grouping", "yardstick")`.
#'
#' @param fn A yardstick metric function or metric set.
#' @param name The name of the metric to place in the `.metric` column
#' of the output.
#' @param aggregate A function to summarize the generated metric set results.
#' The function takes metric set results as the first argument and returns
#' a single numeric giving the `.estimate` value as output. See the Value and
#' Examples sections for example uses.
#' @inheritParams new_class_metric
#'
#' @section Relevant Group Level:
#' Additional arguments can be passed to the function outputted by
#' the function that this function outputs. That is:
#'
#' ```
#' res_fairness <- new_groupwise_metric(...)
#' res_by <- res_fairness(by)
#' res_by(..., additional_arguments_to_aggregate = TRUE)
#' ```
#'
#' For finer control of how groups in `by` are treated, use the
#' `aggregate` argument.
#'
#' @return
#' This function is a
#' [function factory](https://adv-r.hadley.nz/function-factories.html); its
#' output is itself a function. Further, the functions that this function
#' outputs are also function factories. More explicitly, this looks like:
#'
#' ```
#' # a function with similar implementation to `demographic_parity()`:
#' diff_range <- function(x) {diff(range(x$.estimate))}
#'
#' dem_parity <-
#'   new_groupwise_metric(
#'     fn = detection_prevalence,
#'     name = "dem_parity",
#'     aggregate = diff_range
#'   )
#' ```
#'
#' The outputted `dem_parity` is a function that takes one argument, `by`,
#' indicating the data-masked variable giving the sensitive feature.
#'
#' When called with a `by` argument, `dem_parity` will return a yardstick
#' metric function like any other:
#'
#' ```
#' dem_parity_by_gender <- dem_parity(gender)
#' ```
#'
#' Note that `dem_parity` doesn't take any arguments other than `by`, and thus
#' knows nothing about the data it will be applied to other than that it ought
#' to have a column with name `"gender"` in it.
#'
#' The output `dem_parity_by_gender` is a metric function that takes the
#' same arguments as the function supplied as `fn`, in this case
#' `detection_prevalence`. It will thus interface like any other yardstick
#' function except that it will look for a `"gender"` column in
#' the data it's supplied.
#'
#' In addition to the examples below, see the documentation on the
#' return value of fairness metrics like [demographic_parity()],
#' [equal_opportunity()], or [equalized_odds()] to learn more about how the
#' output of this function can be used.
#'
#' @examples
#' data(hpc_cv)
#'
#' # `demographic_parity`, among other fairness metrics,
#' # is generated with `new_groupwise_metric()`:
#' diff_range <- function(x) {diff(range(x$.estimate))}
#' demographic_parity_ <-
#'   new_groupwise_metric(
#'     fn = detection_prevalence,
#'     name = "demographic_parity",
#'     aggregate = diff_range
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
#'   new_groupwise_metric(
#'     fn = detection_prevalence,
#'     name = "demographic_parity_ratio",
#'     aggregate = ratio_range
#'   )
#'
#' @export
new_groupwise_metric <- function(fn, name, aggregate, direction = "minimize") {
  if (is_missing(fn) || !inherits_any(fn, c("metric", "metric_set"))) {
    cli::cli_abort(
      "{.arg fn} must be a metric function or metric set."
    )
  }
  if (is_missing(name) || !is_string(name)) {
    cli::cli_abort(
      "{.arg name} must be a string."
    )
  }
  if (is_missing(aggregate) || !is_function(aggregate)) {
    cli::cli_abort(
      "{.arg aggregate} must be a function."
    )
  }
  arg_match(
    direction,
    values = c("maximize", "minimize", "zero")
  )

  metric_factory <-
    function(by) {
      by_str <- as_string(enexpr(by))
      res <-
        function(data, ...) {
          gp_vars <- dplyr::group_vars(data)

          if (by_str %in% gp_vars) {
            cli::cli_abort(
              "Metric is internally grouped by {.field {by_str}}; grouping
              {.arg data} by {.field {by_str}} is not well-defined."
            )
          }

          # error informatively when `fn` is a metric set; see `eval_safely()`
          data_grouped <- dplyr::group_by(data, {{ by }}, .add = TRUE)
          res <-
            tryCatch(
              fn(data_grouped, ...),
              error = function(cnd) {
                if (!is.null(cnd$parent)) {
                  cnd <- cnd$parent
                }

                cli::cli_abort(conditionMessage(cnd), call = call(name))
              }
            )

          # restore to the grouping structure in the supplied data
          if (length(gp_vars) > 0) {
            res <- dplyr::group_by(res, !!!dplyr::groups(data), .add = FALSE)
          }

          group_rows <- dplyr::group_rows(res)
          group_keys <- dplyr::group_keys(res)
          res <- dplyr::ungroup(res)
          groups <- vec_chop(res, indices = group_rows)
          out <- vector("list", length = length(groups))

          for (i in seq_along(groups)) {
            group <- groups[[i]]

            .estimate <- aggregate(group)

            if (!is_bare_numeric(.estimate)) {
              cli::cli_abort(
                "{.arg aggregate} must return a single numeric value.",
                call = call2("new_groupwise_metric")
              )
            }

            elt_out <- list(
              .metric = name,
              .by = by_str,
              .estimator = group$.estimator[1],
              .estimate = .estimate
            )

            out[[i]] <- tibble::new_tibble(elt_out)
          }

          group_keys <- vctrs::vec_rep_each(group_keys, times = list_sizes(out))
          out <- vec_rbind(!!!out)
          out <- vec_cbind(group_keys, out)

          out
        }
      res <- new_class_metric(res, direction = "minimize")

      structure(
        res,
        direction = direction,
        by = by_str,
        class = groupwise_metric_class(fn)
      )
    }

  structure(metric_factory, class = c("metric_factory", "function"))
}

groupwise_metric_class <- function(fn) {
  if (inherits(fn, "metric")) {
    return(class(fn))
  }

  class(attr(fn, "metrics")[[1]])
}

#' @noRd
#' @export
print.metric_factory <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
format.metric_factory <- function(x, ...) {
  cli::cli_format_method(
    cli::cli_text("A {.help [metric factory](yardstick::new_groupwise_metric)}")
  )
}

diff_range <- function(x) {
  estimates <- x$.estimate

  max(estimates) - min(estimates)
}
