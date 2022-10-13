#' Developer function for summarizing new metrics
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`, and
#' `prob_metric_summarizer()` are useful alongside [metric_vec_template()] for
#' implementing new custom metrics. These functions call the metric function
#' inside `dplyr::summarise()`. `metric_vec_template()` is a generalized
#' function that calls the core implementation of a metric function, and
#' includes a number of checks on the types, lengths, and argument inputs.
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
                                      fn_options = list()) {
  check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  # Explicit handling of length 1 character vectors as column names
  nms <- colnames(data)
  truth <- handle_chr_names(truth, nms)
  estimate <- handle_chr_names(estimate, nms)

  metric_tbl <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(!! truth, metric_class = name),
    .estimate = fn(
      truth = !! truth,
      estimate = !! estimate,
      na_rm = na_rm,
      !!! spliceable_case_weights(case_weights),
      !!! fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
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
                                    fn_options = list()) {
  check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  # Explicit handling of length 1 character vectors as column names
  nms <- colnames(data)
  truth <- handle_chr_names(truth, nms)
  estimate <- handle_chr_names(estimate, nms)

  metric_tbl <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(!! truth, estimator, name),
    .estimate = fn(
      truth = !! truth,
      estimate = !! estimate,
      !!! spliceable_estimator(estimator),
      na_rm = na_rm,
      !!! spliceable_event_level(event_level),
      !!! spliceable_case_weights(case_weights),
      !!! fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
}

#' @rdname metric-summarizers
#' @export
prob_metric_summarizer <- function(name,
                                   fn,
                                   data,
                                   truth,
                                   estimate,
                                   ...,
                                   estimator = NULL,
                                   na_rm = TRUE,
                                   event_level = NULL,
                                   case_weights = NULL,
                                   fn_options = list()) {
  check_dots_empty()

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  # Explicit handling of length 1 character vectors as column names
  nms <- colnames(data)
  truth <- handle_chr_names(truth, nms)
  estimate <- handle_chr_names(estimate, nms)

  metric_tbl <- dplyr::summarise(
    data,
    .metric = name,
    .estimator = finalize_estimator(!! truth, estimator, name),
    .estimate = fn(
      truth = !! truth,
      estimate = !! estimate,
      !!! spliceable_estimator(estimator),
      na_rm = na_rm,
      !!! spliceable_event_level(event_level),
      !!! spliceable_case_weights(case_weights),
      !!! fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
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

validate_case_weights <- function(case_weights, size) {
  if (is.null(case_weights)) {
    return(invisible())
  }

  if (!is.integer(case_weights) && !is.double(case_weights)) {
    abort("`case_weights` must be an integer or double vector.")
  }

  size_case_weights <- length(case_weights)

  if (size_case_weights != size) {
    abort(paste0(
      "`case_weights` (", size_case_weights, ") must have the same ",
      "length as `truth` (", size, ")."
    ))
  }

  invisible()
}

handle_chr_names <- function(x, nms) {
  x_expr <- get_expr(x)

  # Replace character with bare name
  if(is.character(x_expr) && length(x_expr) == 1) {
    # Only replace if it is actually a column name in `data`
    if(x_expr %in% nms) {
      # Replace the quosure with just the name
      # Don't replace the quosure expression, this
      # breaks with dplyr 0.8.0.1 and R <= 3.4.4
      x <- as.name(x_expr)
    }
  }

  x
}

metric_tibbler <- function(.metric, .estimator, .estimate) {
  dplyr::tibble(
    .metric = .metric,
    .estimator = .estimator,
    .estimate = .estimate
  )
}

# if estimator = NULL, we don't want to pass it along
# as an argument. (autoselection will do the work for us)
# splicing in an empty list essentially is equivalent
# to splicing in nothing.
spliceable_estimator <- function(estimator) {
  if (!is.null(estimator)) {
    return(list(estimator = estimator))
  }
  else {
    return(list())
  }
}

spliceable_event_level <- function(event_level) {
  if (!is.null(event_level)) {
    return(list(event_level = event_level))
  }
  else {
    return(list())
  }
}

spliceable_case_weights <- function(case_weights) {
  if (quo_is_null(case_weights)) {
    return(list())
  }

  list(case_weights = case_weights)
}


