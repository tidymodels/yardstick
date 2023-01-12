#' Developer function for summarizing new metrics
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`, and
#' `prob_metric_summarizer()` are useful alongside [check_metric] and
#' [yardstick_remove_missing] for implementing new custom metrics. These
#' functions call the metric function inside `dplyr::summarise()`. See [Custom
#' performance metrics](https://www.tidymodels.org/learn/develop/metrics/) for
#' more information.
#'
#' @details
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`, and
#' `prob_metric_summarizer()` are generally called from the data frame version
#' of your metric function. It knows how to call your metric over grouped data
#' frames and returns a `tibble` consistent with other metrics.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @param name A single character representing the name of the metric to
#' use in the `tibble` output. This will be modified to include the type
#' of averaging if appropriate.
#'
#' @param fn The vector version of your custom metric function. It
#' generally takes `truth`, `estimate`, `na_rm`, and any other extra arguments
#' needed to calculate the metric.
#'
#' @param data The data frame with `truth` and `estimate` columns passed
#' in from the data frame version of your metric function that called
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`, or
#' `prob_metric_summarizer()`
#'
#' @param truth The unquoted column name corresponding to the `truth` column.
#'
#' @param estimate Generally, the unquoted column name corresponding to
#' the `estimate` column. For metrics that take multiple columns through `...`
#' like class probability metrics, this is a result of [dots_to_estimate()].
#'
#' @param estimator This can either be `NULL` for the default auto-selection of
#' averaging (`"binary"` or `"macro"`), or a single character to pass along to
#' the metric implementation describing the kind of averaging to use.
#'
#' @param .time A `numeric` vector. Indicating time points where dynamic
#' survival metrics should be calculated at.
#'
#' @param na_rm A `logical` value indicating whether `NA` values should be
#' stripped before the computation proceeds. The removal is executed in
#' [yardstick_remove_missing()].
#'
#' @param event_level This can either be `NULL` to use the default `event_level`
#' value of the `fn` or a single string of either `"first"` or `"second"`
#' to pass along describing which level should be considered the "event".
#'
#' @param case_weights For metrics supporting case weights, an unquoted
#' column name corresponding to case weights can be passed here. If not `NULL`,
#' the case weights will be passed on to `fn` as the named argument
#' `case_weights`.
#'
#' @param fn_options A named list of metric specific options. These
#' are spliced into the metric function call using `!!!` from `rlang`. The
#' default results in nothing being spliced into the call.
#'
#' @seealso [check_metric] [yardstick_remove_missing] [finalize_estimator()] [dots_to_estimate()]
#'
#' @name metric-summarizers
NULL

#' @rdname metric-summarizers
#' @export
numeric_metric_summarizer <- function(name,
                                      fn,
                                      data,
                                      truth,
                                      estimate,
                                      ...,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      fn_options = list(),
                                      error_call = caller_env()) {
  rlang::check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth",
    error_call = error_call
  )
  estimate <- yardstick_eval_select(
    expr = estimate,
    data = data,
    arg = "estimate",
    error_call = error_call
  )

  if (!quo_is_null(case_weights)) {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )

    case_weights <- expr(.data[[!!case_weights]])
  }

  out <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(.data[[truth]], metric_class = name),
    .estimate = fn(
      truth = .data[[truth]],
      estimate = .data[[estimate]],
      case_weights = !!case_weights,
      na_rm = na_rm,
      !!!fn_options
    )
  )

  dplyr::as_tibble(out)
}

#' @rdname metric-summarizers
#' @export
class_metric_summarizer <- function(name,
                                    fn,
                                    data,
                                    truth,
                                    estimate,
                                    ...,
                                    estimator = NULL,
                                    na_rm = TRUE,
                                    event_level = NULL,
                                    case_weights = NULL,
                                    fn_options = list(),
                                    error_call = caller_env()) {
  rlang::check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth",
    error_call = error_call
  )
  estimate <- yardstick_eval_select(
    expr = estimate,
    data = data,
    arg = "estimate",
    error_call = error_call
  )

  if (!quo_is_null(case_weights)) {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )

    case_weights <- expr(.data[[!!case_weights]])
  }

  out <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(.data[[truth]], estimator, name),
    .estimate = fn(
      truth = .data[[truth]],
      estimate = .data[[estimate]],
      case_weights = !!case_weights,
      na_rm = na_rm,
      !!! spliceable_argument(estimator, "estimator"),
      !!! spliceable_argument(event_level, "event_level"),
      !!! fn_options
    )
  )

  dplyr::as_tibble(out)
}

#' @rdname metric-summarizers
#' @export
prob_metric_summarizer <- function(name,
                                   fn,
                                   data,
                                   truth,
                                   ...,
                                   estimator = NULL,
                                   na_rm = TRUE,
                                   event_level = NULL,
                                   case_weights = NULL,
                                   fn_options = list(),
                                   error_call = caller_env()) {
  truth <- enquo(truth)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth",
    error_call = error_call
  )
  estimate <- yardstick_eval_select_dots(
    ...,
    data = data,
    error_call = error_call
  )

  if (!quo_is_null(case_weights)) {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )

    case_weights <- expr(.data[[!!case_weights]])
  }

  out <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(.data[[truth]], estimator, name),
    .estimate = fn(
      truth = .data[[truth]],
      estimate = {
        # TODO: Use `dplyr::pick()` from dplyr 1.1.0
        estimate <- dplyr::across(tidyselect::all_of(estimate), .fns = identity)
        prob_estimate_convert(estimate)
      },
      case_weights = !!case_weights,
      na_rm = na_rm,
      !!! spliceable_argument(estimator, "estimator"),
      !!! spliceable_argument(event_level, "event_level"),
      !!! fn_options
    )
  )

  dplyr::as_tibble(out)
}

#' @rdname metric-summarizers
#' @export
dynamic_survival_metric_summarizer <- function(name,
                                           fn,
                                           data,
                                           truth,
                                           estimate,
                                           .time,
                                           ...,
                                           na_rm = TRUE,
                                           case_weights = NULL,
                                           fn_options = list(),
                                           error_call = caller_env()) {
  rlang::check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth",
    error_call = error_call
  )
  estimate <- yardstick_eval_select(
    expr = estimate,
    data = data,
    arg = "estimate",
    error_call = error_call
  )

  if (!quo_is_null(case_weights)) {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )

    case_weights <- expr(.data[[!!case_weights]])
  }

  out <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(.data[[truth]], metric_class = name),
    .estimate = fn(
      truth = .data[[truth]],
      estimate = .data[[estimate]],
      .time = .time,
      case_weights = !!case_weights,
      na_rm = na_rm,
      !!!fn_options
    ),
    .time = .time
  )
  out <- tidyr::nest(out, .estimate = c(.time, .estimate))

  dplyr::as_tibble(out)
}


prob_estimate_convert <- function(estimate) {
  if (!is.data.frame(estimate)) {
    abort("`estimate` should be a data frame.", .internal = TRUE)
  }

  n_estimate <- ncol(estimate)

  if (n_estimate == 0L) {
    abort("`estimate` should have errored during tidy-selection.", .internal = TRUE)
  } else if (n_estimate == 1L) {
    # Unwrap single column `estimate`s
    estimate[[1L]]
  } else {
    # Otherwise multiclass case requires a matrix
    as.matrix(estimate)
  }
}

metric_tibbler <- function(.metric, .estimator, .estimate) {
  dplyr::tibble(
    .metric = .metric,
    .estimator = .estimator,
    .estimate = .estimate
  )
}

spliceable_argument <- function(x, name) {
  if (is.null(x)) {
    return(list())
  }

  out <- list(x)
  names(out) <- name

  out
}

yardstick_eval_select <- function(expr,
                                  data,
                                  arg,
                                  ...,
                                  error_call = caller_env()) {
  check_dots_empty()

  out <- tidyselect::eval_select(
    expr = expr,
    data = data,
    allow_predicates = FALSE,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )
  out <- names(out)

  if (length(out) != 1L) {
    message <- paste0("`", arg, "` must select exactly 1 column from `data`.")
    abort(message, call = error_call)
  }

  out
}

yardstick_eval_select_dots <- function(...,
                                       data,
                                       error_call = caller_env()) {
  out <- tidyselect::eval_select(
    expr = expr(c(...)),
    data = data,
    allow_predicates = FALSE,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )

  out <- names(out)

  out
}
