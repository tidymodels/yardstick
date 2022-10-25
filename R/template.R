#' Developer function for summarizing new metrics
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`, and
#' `prob_metric_summarizer()` are useful alongside [metric-vec_template()] for
#' implementing new custom metrics. These functions call the metric function
#' inside `dplyr::summarise()`. The [metric-vec_template()] functions are
#' generalized function that calls the core implementation of a metric function.
#' See [Custom performance
#' metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
#' information.
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
#' @param na_rm A `logical` value indicating whether `NA` values should be
#' stripped before the computation proceeds. The removal is executed in
#' `metric_vec_template()`.
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
#' @seealso [metric_vec_template()] [finalize_estimator()] [dots_to_estimate()]
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

#' Developer function for calling new metrics
#'
#' `class_metric_vec_template()`, `numeric_metric_vec_template()`, and
#' `prob_metric_vec_template()` are useful alongside the [metric-summarizers()]
#' functions for implementing new custom metrics. [metric-summarizers()] calls
#' the metric function inside `dplyr::summarise()`.
#' `class_metric_vec_template()`, `numeric_metric_vec_template()`, and
#' `prob_metric_vec_template()` are generalized function that calls the core
#' implementation of a metric function.
#'
#' @param metric_impl The core implementation function of your custom metric.
#'   This core implementation function is generally defined inside the vector
#'   method of your metric function.
#'
#' @param truth The realized vector of `truth`. This is either a factor or a
#'   numeric.
#'
#' @param estimate The realized `estimate` result. This is either a numeric
#'   vector, a factor vector, or a numeric matrix (in the case of multiple class
#'   probability columns) depending on your metric function.
#'
#' @param na_rm A `logical` value indicating whether `NA` values should be
#'   stripped before the computation proceeds. `NA` values are removed before
#'   getting to your core implementation function so you do not have to worry
#'   about handling them yourself. If `na_rm=FALSE` and any `NA` values exist,
#'   then `NA` is automatically returned.
#'
#' @param case_weights the realized case weights, as a numeric vector. This must
#'   be the same length as `truth`, and will be considered in the `na_rm`
#'   checks. If supplied, this will be passed on to `metric_impl` as the named
#'   argument `case_weights`.
#'
#' @param ... Extra arguments to your core metric function, `metric_impl`, can
#'   technically be passed here, but generally the extra args are added through
#'   R's scoping rules because the core metric function is created on the fly
#'   when the vector method is called.
#'
#' @details
#'
#' `class_metric_vec_template()`, `numeric_metric_vec_template()`, and
#' `prob_metric_vec_template()` are called from the vector implementation of
#' your metric. Also defined inside your vector implementation is a separate
#' function performing the core implementation of the metric function. This
#' core function is passed along as `metric_impl`.
#'
#' @seealso [metric-summarizers()] [finalize_estimator()] [dots_to_estimate()]
#'
#' @name metric-vec_template
NULL

#' @rdname metric-vec_template
#' @export
numeric_metric_vec_template <- function(metric_impl,
                                        truth,
                                        estimate,
                                        na_rm = TRUE,
                                        case_weights = NULL,
                                        ...) {
  validate_case_weights(case_weights, size = length(truth))

  if (na_rm) {
    complete_cases <- stats::complete.cases(truth, estimate, case_weights)
    truth <- truth[complete_cases]

    estimate <- estimate[complete_cases]
    case_weights <- case_weights[complete_cases]
  } else {
    any_na <-
      anyNA(truth) ||
      anyNA(estimate) ||
      (!is.null(case_weights) && anyNA(case_weights))

    # return NA if any NA
    if (any_na) {
      return(NA_real_)
    }
  }

  metric_impl(truth = truth, estimate = estimate, case_weights = case_weights, ...)
}

#' @rdname metric-vec_template
#' @export
class_metric_vec_template <- function(metric_impl,
                                      truth,
                                      estimate,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      ...) {
  validate_case_weights(case_weights, size = length(truth))

  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }

  if (na_rm) {
    complete_cases <- stats::complete.cases(truth, estimate, case_weights)
    truth <- truth[complete_cases]

    estimate <- estimate[complete_cases]
    case_weights <- case_weights[complete_cases]
  } else {
    any_na <-
      anyNA(truth) ||
      anyNA(estimate) ||
      (!is.null(case_weights) && anyNA(case_weights))

    # return NA if any NA
    if (any_na) {
      return(NA_real_)
    }
  }

  metric_impl(truth = truth, estimate = estimate, case_weights = case_weights, ...)
}

#' @rdname metric-vec_template
#' @export
prob_metric_vec_template <- function(metric_impl,
                                     truth,
                                     estimate,
                                     na_rm = TRUE,
                                     case_weights = NULL,
                                     ...) {
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }

  validate_case_weights(case_weights, size = length(truth))

  if (na_rm) {
    complete_cases <- stats::complete.cases(truth, estimate, case_weights)
    truth <- truth[complete_cases]

    if (is.matrix(estimate)) {
      estimate <- estimate[complete_cases, , drop = FALSE]
    } else {
      estimate <- estimate[complete_cases]
    }

    case_weights <- case_weights[complete_cases]
  } else {
    any_na <-
      anyNA(truth) ||
      anyNA(estimate) ||
      (!is.null(case_weights) && anyNA(case_weights))

    # return NA if any NA
    if (any_na) {
      return(NA_real_)
    }
  }

  metric_impl(truth = truth, estimate = estimate, case_weights = case_weights, ...)
}

#' Developer function for calling new metrics
#'
#' `metric_vec_template()` is useful alongside [metric_summarizer()] for
#' implementing new custom metrics. `metric_summarizer()` calls the metric
#' function inside `dplyr::summarise()`. `metric_vec_template()` is a
#' generalized function that calls the core implementation of a metric function,
#' and includes a number of checks on the types, lengths, and argument inputs.
#'
#' @param metric_impl The core implementation function of your custom metric.
#' This core implementation function is generally defined inside the vector
#' method of your metric function.
#'
#' @param truth The realized vector of `truth`. This is either a factor
#' or a numeric.
#'
#' @param estimate The realized `estimate` result. This is either a numeric
#' vector, a factor vector, or a numeric matrix (in the case of multiple
#' class probability columns) depending on your metric function.
#'
#' @param na_rm A `logical` value indicating whether `NA` values should be
#' stripped before the computation proceeds. `NA` values are removed
#' before getting to your core implementation function so you do not have to
#' worry about handling them yourself. If `na_rm=FALSE` and any `NA` values
#' exist, then `NA` is automatically returned.
#'
#' @param cls A character vector of length 1 or 2 corresponding to the
#' class that `truth` and `estimate` should be, respectively. If `truth` and
#' `estimate` are of the same class, just supply a vector of length 1. If
#' they are different, supply a vector of length 2. For matrices, it is best
#' to supply `"numeric"` as the class to check here.
#'
#' @param estimator The type of averaging to use. By this point, the averaging
#' type should be finalized, so this should be a character vector of length 1\.
#' By default, this character value is required to be one of: `"binary"`,
#' `"macro"`, `"micro"`, or `"macro_weighted"`. If your metric allows more
#' or less averaging methods, override this with `averaging_override`.
#'
#' @param case_weights Optionally, the realized case weights, as a numeric
#' vector. This must be the same length as `truth`, and will be considered in
#' the `na_rm` checks. If supplied, this will be passed on to `metric_impl` as
#' the named argument `case_weights`.
#'
#' @param ... Extra arguments to your core metric function, `metric_impl`, can
#' technically be passed here, but generally the extra args are added through
#' R's scoping rules because the core metric function is created on the fly
#' when the vector method is called.
#'
#' @details
#'
#' `metric_vec_template()` is called from the vector implementation of your
#' metric. Also defined inside your vector implementation is a separate
#' function performing the core implementation of the metric function. This
#' core function is passed along to `metric_vec_template()` as `metric_impl`.
#'
#' @seealso [metric_summarizer()] [finalize_estimator()] [dots_to_estimate()]
#'
#' @export
metric_vec_template <- function(metric_impl,
                                truth,
                                estimate,
                                na_rm = TRUE,
                                cls = "numeric",
                                estimator = NULL,
                                case_weights = NULL,
                                ...) {
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }

  validate_truth_estimate_checks(truth, estimate, cls, estimator)
  validate_case_weights(case_weights, size = length(truth))

  has_case_weights <- !is.null(case_weights)

  if (na_rm) {
    complete_cases <- stats::complete.cases(truth, estimate, case_weights)
    truth <- truth[complete_cases]

    if (is.matrix(estimate)) {
      estimate <- estimate[complete_cases, , drop = FALSE]
    } else {
      estimate <- estimate[complete_cases]
    }

    if (has_case_weights) {
      case_weights <- case_weights[complete_cases]
    }
  } else {
    any_na <-
      anyNA(truth) ||
      anyNA(estimate) ||
      (has_case_weights && anyNA(case_weights))

    # return NA if any NA
    if (any_na) {
      return(NA_real_)
    }
  }

  if (has_case_weights) {
    metric_impl(truth = truth, estimate = estimate, case_weights = case_weights, ...)
  } else {
    # Assume signature doesn't have `case_weights =`
    metric_impl(truth = truth, estimate = estimate, ...)
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
