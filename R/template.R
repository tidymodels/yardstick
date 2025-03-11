#' Developer function for summarizing new metrics
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`,
#' `prob_metric_summarizer()`, `curve_metric_summarizer()`,
#' `dynamic_survival_metric_summarizer()`, and
#' `static_survival_metric_summarizer()` are useful alongside [check_metric] and
#' [yardstick_remove_missing] for implementing new custom metrics. These
#' functions call the metric function inside `dplyr::summarise()` or
#' `dplyr::reframe()` for `curve_metric_summarizer()`. See [Custom performance
#' metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
#' information.
#'
#' @details
#'
#' `numeric_metric_summarizer()`, `class_metric_summarizer()`,
#' `prob_metric_summarizer()`, `curve_metric_summarizer()`,
#' `dynamic_survival_metric_summarizer()`, and
#' `dynamic_survival_metric_summarizer()` are generally called from the data
#' frame version of your metric function. It knows how to call your metric over
#' grouped data frames and returns a `tibble` consistent with other metrics.
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
#' @param data The data frame with `truth` and `estimate` columns passed in from
#'   the data frame version of your metric function that called
#'   `numeric_metric_summarizer()`, `class_metric_summarizer()`,
#'   `prob_metric_summarizer()`, `curve_metric_summarizer()`,
#'   `dynamic_survival_metric_summarizer()`, or
#'   `static_survival_metric_summarizer()`.
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
numeric_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
  check_dots_empty(call = error_call)

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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        metric_class = name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
class_metric_summarizer <- function(
  name,
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
  error_call = caller_env()
) {
  check_dots_empty(call = error_call)

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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        estimator,
        name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!spliceable_argument(estimator, "estimator"),
            !!!spliceable_argument(event_level, "event_level"),
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
prob_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- prob_estimate_convert(group[estimate])

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        estimator,
        name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!spliceable_argument(estimator, "estimator"),
            !!!spliceable_argument(event_level, "event_level"),
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
ordered_prob_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- prob_estimate_convert(group[estimate])

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        estimator,
        name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!spliceable_argument(estimator, "estimator"),
            !!!spliceable_argument(event_level, "event_level"),
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
curve_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- prob_estimate_convert(group[estimate])

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        estimator,
        name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!spliceable_argument(estimator, "estimator"),
            !!!spliceable_argument(event_level, "event_level"),
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    elt_out <- vec_recycle_common(!!!elt_out)
    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
dynamic_survival_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        metric_class = name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    elt_out <- vec_recycle_common(!!!elt_out)
    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  if (inherits(out$.estimate, "tbl_df")) {
    out <- tidyr::unnest(out, .estimate)
  }

  out
}

#' @rdname metric-summarizers
#' @export
static_survival_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
  check_dots_empty(call = error_call)

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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        metric_class = name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

#' @rdname metric-summarizers
#' @export
curve_survival_metric_summarizer <- function(
  name,
  fn,
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
) {
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

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- prob_estimate_convert(group[estimate])

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    elt_out <- list(
      .metric = name,
      .estimator = finalize_estimator(
        group_truth,
        metric_class = name,
        call = error_call
      ),
      .estimate = inject(
        withCallingHandlers(
          fn(
            truth = group_truth,
            estimate = group_estimate,
            case_weights = group_case_weights,
            na_rm = na_rm,
            !!!fn_options
          ),
          error = function(cnd) {
            cnd$call <- error_call
            cnd_signal(cnd)
          }
        )
      )
    )

    elt_out <- vec_recycle_common(!!!elt_out)
    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vec_rep_each(group_keys, times = list_sizes(out))
  out <- vec_rbind(!!!out)
  out <- vec_cbind(group_keys, out)

  out
}

prob_estimate_convert <- function(estimate) {
  check_data_frame(estimate, .internal = TRUE)

  n_estimate <- ncol(estimate)

  if (n_estimate == 0L) {
    # should be unreachable
    cli::cli_abort(
      "{.arg estimate} should have errored during tidy-selection.",
      .internal = TRUE
    )
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

yardstick_eval_select <- function(
  expr,
  data,
  arg,
  ...,
  error_call = caller_env()
) {
  check_dots_empty(call = error_call)

  if (!quo_is_missing(expr) && inherits(quo_get_expr(expr), "name")) {
    expr_name <- as_name(expr)

    if (is_known_selection(expr_name)) {
      return(get_known_selection(expr_name))
    }
  }

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
    cli::cli_abort(
      "{.arg {arg}} must select exactly 1 column from {.arg data}, 
      not {length(out)}.",
      call = error_call
    )
  }

  set_known_selection(as_name(expr), out)

  out
}

yardstick_eval_select_dots <- function(..., data, error_call = caller_env()) {
  expr <- quo(!!substitute(...))

  if (
    !quo_is_missing(expr) && inherits(quo_get_expr(expr), c("name", "call"))
  ) {
    expr_label <- as_label(expr)

    if (is_known_selection(expr_label)) {
      return(get_known_selection(expr_label))
    }
  }

  if ("estimate" %in% names(match.call(expand.dots = FALSE)$...)) {
    cli::cli_abort(
      c(
        x = "This metric doesn't use the {.arg estimate} argument.",
        i = "Specify the columns without {.code estimate = }."
      ),
      call = error_call
    )
  }

  out <- tidyselect::eval_select(
    expr = expr(c(...)),
    data = data,
    allow_predicates = FALSE,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )

  out <- names(out)

  set_known_selection(as_label(expr), out)

  out
}

# store known selections (#428) ------------------------------------------------
is_known_selection <- function(expr_name) {
  if (!catalog_is_available() || !in_tuning_env()) {
    return(FALSE)
  }

  known_selections <- ns_env("tune")$tune_env$known_selections

  expr_name %in% names(known_selections)
}

get_known_selection <- function(expr_name) {
  known_selections <- ns_env("tune")$tune_env$known_selections

  known_selections[[expr_name]]
}

set_known_selection <- function(expr_name, out) {
  if (!catalog_is_available() || !in_tuning_env()) {
    return(invisible())
  }

  tune_env <- ns_env("tune")$tune_env

  if (is.null(tune_env$known_selections)) {
    init_known_selection(tune_env)
  }

  tune_env$known_selections[[expr_name]] <- out

  invisible()
}

init_known_selection <- function(tune_env) {
  withr::defer(
    env_bind(tune_env, known_selections = NULL),
    envir = tune_env$progress_env
  )
}

# `catalog_is_available()` defines the per-session condition for whether we
# can store known selections, while `in_tuning_env()` defines the
# per-tuning instance of the condition. we want to store known selections:
# 1) when tune's cataloging machinery is available
# 2) when called inside of a tuning env, regardless of whether the catalog is
#    being used to log errors.
# notably, we don't do so when yardstick functions are called outside of
# tune, in which case the overhead is `in_tuning_env()` after the first call.
#
# set up as a function with local variables so that we only have to run
# `is_installed()` and `packageVersion()` once.
catalog_is_available <-
  local({
    tune_is_installed <- NULL
    tune_version_is_sufficient <- FALSE

    function() {
      if (is.null(tune_is_installed)) {
        tune_is_installed <<- is_installed("tune")
        if (tune_is_installed) {
          tune_version_is_sufficient <<- utils::packageVersion("tune") > "1.1.0"
        }
      }
      tune_is_installed && tune_version_is_sufficient
    }
  })

in_tuning_env <- function(tune_env = ns_env("tune")$tune_env) {
  isTRUE(tune_env$progress_active)
}
