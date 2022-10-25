#' Developer function for summarizing new metrics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `metric_summarizer()` has been soft-deprecated as of yardstick 1.2.0. Please
#' switch to use [class_metric_summarizer()], [numeric_metric_summarizer()], or
#' [prob_metric_summarizer()].
#'
#' @param metric_nm A single character representing the name of the metric to
#' use in the `tibble` output. This will be modified to include the type
#' of averaging if appropriate.
#'
#' @param metric_fn The vector version of your custom metric function. It
#' generally takes `truth`, `estimate`, `na_rm`, and any other extra arguments
#' needed to calculate the metric.
#'
#' @param data The data frame with `truth` and `estimate` columns passed
#' in from the data frame version of your metric function that called
#' `metric_summarizer()`.
#'
#' @param truth The unquoted column name corresponding to the `truth` column.
#'
#' @param estimate Generally, the unquoted column name corresponding to
#' the `estimate` column. For metrics that take multiple columns through `...`
#' like class probability metrics, this is a result of [dots_to_estimate()].
#'
#' @param estimator For numeric metrics, this is left as `NULL` so averaging
#' is not passed on to the metric function implementation. For classification
#' metrics, this can either be `NULL` for the default auto-selection of
#' averaging (`"binary"` or `"macro"`), or a single character to pass along
#' to the metric implementation describing the kind of averaging to use.
#'
#' @param na_rm A `logical` value indicating whether `NA` values should be
#' stripped before the computation proceeds. The removal is executed in
#' `metric_vec_template()`.
#'
#' @param event_level For numeric metrics, this is left as `NULL` to prevent
#' it from being passed on to the metric function implementation. For
#' classification metrics, this can either be `NULL` to use the default
#' `event_level` value of the `metric_fn` or a single string of either
#' `"first"` or `"second"` to pass along describing which level should be
#' considered the "event".
#'
#' @param case_weights For metrics supporting case weights, an unquoted
#' column name corresponding to case weights can be passed here. If not `NULL`,
#' the case weights will be passed on to `metric_fn` as the named argument
#' `case_weights`.
#'
#' @param ... Currently not used. Metric specific options are passed in
#' through `metric_fn_options`.
#'
#' @param metric_fn_options A named list of metric specific options. These
#' are spliced into the metric function call using `!!!` from `rlang`. The
#' default results in nothing being spliced into the call.
#'
#' @keywords internal
#' @export
metric_summarizer <- function(metric_nm,
                              metric_fn,
                              data,
                              truth,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = NULL,
                              case_weights = NULL,
                              ...,
                              metric_fn_options = list()) {
  lifecycle::deprecate_soft(
    when = "1.2.0",
    what = "metric_summarizer()",
    with = I("`numeric_metric_summarizer()`, `class_metric_summarizer()`, or `prob_metric_summarizer()`")
  )
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  validate_not_missing(truth, "truth")
  validate_not_missing(estimate, "estimate")

  # Explicit handling of length 1 character vectors as column names
  nms <- colnames(data)
  truth <- handle_chr_names(truth, nms)
  estimate <- handle_chr_names(estimate, nms)

  finalize_estimator_expr <- rlang::expr(
    finalize_estimator(!! truth, estimator, metric_nm)
  )

  metric_tbl <- dplyr::summarise(
    data,
    .metric = metric_nm,
    .estimator = eval_tidy(finalize_estimator_expr),
    .estimate = metric_fn(
      truth = !! truth,
      estimate = !! estimate,
      !!! spliceable_argument(estimator, "estimator"),
      na_rm = na_rm,
      !!! spliceable_argument(event_level, "event_level"),
      !!! spliceable_case_weights(case_weights),
      !!! metric_fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
}

# ------------------------------------------------------------------------------
# Utilities

validate_not_missing <- function(x, nm) {
  if(rlang::quo_is_missing(x)) {
    abort(paste0(
      "`", nm, "` ",
      "is missing and must be supplied."
    ))
  }
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

spliceable_case_weights <- function(case_weights) {
  if (rlang::quo_is_null(case_weights)) {
    return(list())
  }

  list(case_weights = case_weights)
}

#' Developer function for calling new metrics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `metric_vec_template()` has been soft-deprecated as of yardstick 1.2.0.
#' Please switch to use [class_metric_vec_template()],
#' [numeric_metric_vec_template()], or [prob_metric_vec_template()].
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
#' @keywords internal
#' @export
metric_vec_template <- function(metric_impl,
                                truth,
                                estimate,
                                na_rm = TRUE,
                                cls = "numeric",
                                estimator = NULL,
                                case_weights = NULL,
                                ...) {
  lifecycle::deprecate_soft(
    when = "1.2.0",
    what = "metric_vec_template()",
    with = I("`numeric_metric_vec_template()`, `class_metric_vec_template()`, or `prob_metric_vec_template()`")
  )
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
