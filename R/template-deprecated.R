#' Developer function for summarizing new metrics
#'
#' `metric_summarizer()` has been soft-deprecated as of yardstick 1.2.0. Please
#' switch to use [class_metric_summarizer()], [numeric_metric_summarizer()], or
#' [prob_metric_summarizer()].
#'
#' @details
#'
#' `metric_summarizer()` is generally called from the data frame version
#' of your metric function. It knows how to call your metric over grouped data
#' frames and returns a `tibble` consistent with other metrics.
#'
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
#' @seealso [metric_vec_template()] [finalize_estimator()] [dots_to_estimate()]
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
  signal_soft_deprecated(
    paste(
      "`metric_summarizer()` has been soft-deprecated as of version 1.2.0.",
      "Please see `?metric_summarizer()` for further instructions."
    )
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
      !!! spliceable_estimator(estimator),
      na_rm = na_rm,
      !!! spliceable_event_level(event_level),
      !!! spliceable_case_weights(case_weights),
      !!! metric_fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
}
