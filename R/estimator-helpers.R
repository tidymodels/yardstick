#' @section Weight Calculation:
#' `get_weights()` accepts a confusion matrix and an `estimator` of type
#' `"macro"`, `"micro"`, or `"macro_weighted"` and returns the correct weights.
#' It is useful when creating multiclass metrics.
#'
#' @export
#' @rdname developer-helpers
#' @param data A table with truth values as columns and predicted values
#' as rows.
get_weights <- function(data, estimator) {
  if (estimator == "macro") {
    n <- ncol(data)
    rep(1 / n, times = n)
  } else if (estimator == "micro") {
    1
  } else if (estimator == "macro_weighted") {
    .col_sums <- colSums(data)
    .col_sums / sum(.col_sums)
  } else {
    cli::cli_abort(
      "{.arg estimator} type {.val {estimator}} is unknown."
    )
  }
}

# ------------------------------------------------------------------------------

#' @section Estimator Selection:
#'
#' `finalize_estimator()` is the engine for auto-selection of `estimator` based
#' on the type of `x`. Generally `x` is the `truth` column. This function
#' is called from the vector method of your metric.
#'
#' `finalize_estimator_internal()` is an S3 generic that you should extend for
#'  your metric if it does not implement _only_ the following estimator types:
#'  `"binary"`, `"macro"`, `"micro"`, and `"macro_weighted"`.
#'  If your metric does support all of these, the default version of
#'  `finalize_estimator_internal()` will autoselect `estimator` appropriately.
#'  If you need to create a method, it should take the form:
#' `finalize_estimator_internal.metric_name`. Your method for
#' `finalize_estimator_internal()` should do two things:
#'
#' 1) If `estimator` is `NULL`, autoselect the `estimator` based on the
#' type of `x` and return a single character for the `estimator`.
#'
#' 2) If `estimator` is not `NULL`, validate that it is an allowed `estimator`
#' for your metric and return it.
#'
#' If you are using the default for `finalize_estimator_internal()`, the
#' `estimator` is selected using the following heuristics:
#'
#' 1) If `estimator` is not `NULL`, it is validated and returned immediately
#' as no auto-selection is needed.
#'
#' 2) If `x` is a:
#'
#'    * `factor` - Then `"binary"` is returned if it has 2 levels, otherwise
#'      `"macro"` is returned.
#'
#'    * `numeric` - Then `"binary"` is returned.
#'
#'    * `table` - Then `"binary"` is returned if it has 2 columns, otherwise
#'      `"macro"` is returned. This is useful if you have `table` methods.
#'
#'    * `matrix` - Then `"macro"` is returned.
#'
#' @rdname developer-helpers
#'
#' @inheritParams rlang::args_error_context
#'
#' @param metric_class A single character of the name of the metric to autoselect
#' the estimator for. This should match the method name created for
#' `finalize_estimator_internal()`.
#'
#' @param x The column used to autoselect the estimator. This is generally
#' the `truth` column, but can also be a table if your metric has table methods.
#'
#' @param estimator Either `NULL` for auto-selection, or a single character
#' for the type of estimator to use.
#'
#' @seealso [metric-summarizers] [check_metric] [yardstick_remove_missing]
#'
#' @export
finalize_estimator <- function(
  x,
  estimator = NULL,
  metric_class = "default",
  call = caller_env()
) {
  metric_dispatcher <- make_dummy(metric_class)
  finalize_estimator_internal(metric_dispatcher, x, estimator, call = call)
}

#' @rdname developer-helpers
#' @param metric_dispatcher A simple dummy object with the class provided to
#' `metric_class`. This is created and passed along for you.
#' @export
finalize_estimator_internal <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  UseMethod("finalize_estimator_internal")
}

#' @export
finalize_estimator_internal.default <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  finalize_estimator_default(x, estimator, call = call)
}

# Accuracy, Kappa, Mean Log Loss, and MCC have natural multiclass extensions.
# Additionally, they all produce the same results regardless of which level
# is considered the "event". Because of this, the user cannot set the estimator,
# and it should only be "binary" or "multiclass"
#' @export
finalize_estimator_internal.accuracy <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  if (is_multiclass(x)) {
    "multiclass"
  } else {
    "binary"
  }
}

#' @export
finalize_estimator_internal.kap <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.mcc <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.mn_log_loss <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.brier_class <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.ranked_prob_score <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  "multiclass"
}

# Classification cost extends naturally to multiclass and produce the same
# result regardless of the "event" level.
#' @export
finalize_estimator_internal.classification_cost <- finalize_estimator_internal.accuracy

# Curve methods don't use the estimator when printing, but do dispatch
# off it to determine whether to do one-vs-all or not
#' @export
finalize_estimator_internal.gain_curve <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.lift_curve <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.roc_curve <- finalize_estimator_internal.accuracy

#' @export
finalize_estimator_internal.pr_curve <- finalize_estimator_internal.accuracy

# Hand Till method is the "best" multiclass extension to me
# because it is immune to class imbalance like binary roc_auc
#' @export
finalize_estimator_internal.roc_auc <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  validate_estimator(
    estimator = estimator,
    estimator_override = c("binary", "macro", "macro_weighted", "hand_till")
  )

  if (!is.null(estimator)) {
    return(estimator)
  }

  if (is_multiclass(x)) {
    "hand_till"
  } else {
    "binary"
  }
}

# PR AUC and Gain Capture don't have micro methods currently
#' @export
finalize_estimator_internal.pr_auc <- function(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
) {
  validate_estimator(
    estimator = estimator,
    estimator_override = c("binary", "macro", "macro_weighted")
  )

  if (!is.null(estimator)) {
    return(estimator)
  }

  if (is_multiclass(x)) {
    "macro"
  } else {
    "binary"
  }
}

#' @export
finalize_estimator_internal.gain_capture <- finalize_estimator_internal.pr_auc

# Default ----------------------------------------------------------------------

finalize_estimator_default <- function(x, estimator, call = caller_env()) {
  if (!is.null(estimator)) {
    validate_estimator(estimator, call = call)
    return(estimator)
  }
  UseMethod("finalize_estimator_default")
}

#' @export
finalize_estimator_default.default <- function(
  x,
  estimator,
  call = caller_env()
) {
  "binary"
}

#' @export
finalize_estimator_default.matrix <- function(
  x,
  estimator,
  call = caller_env()
) {
  "macro"
}

# Covers all numeric metric functions
#' @export
finalize_estimator_default.numeric <- function(
  x,
  estimator,
  call = caller_env()
) {
  "standard"
}

# Covers all dynamic survival functions
#' @export
finalize_estimator_default.Surv <- function(x, estimator, call = caller_env()) {
  "standard"
}

#' @export
finalize_estimator_default.table <- function(
  x,
  estimator,
  call = caller_env()
) {
  if (is_multiclass(x)) {
    "macro"
  } else {
    "binary"
  }
}

#' @export
finalize_estimator_default.factor <- function(
  x,
  estimator,
  call = caller_env()
) {
  if (is_multiclass(x)) {
    "macro"
  } else {
    "binary"
  }
}

# Util -------------------------------------------------------------------------

make_dummy <- function(metric_class) {
  structure(list(), class = metric_class)
}

is_multiclass <- function(x) {
  UseMethod("is_multiclass")
}

#' @export
is_multiclass.default <- function(x) {
  # dont throw a error here
  # this case should only happen if x is an
  # unknown type, and better error catching
  # is done later to return a good error message
  FALSE
}

#' @export
is_multiclass.table <- function(x) {
  n_col <- ncol(x)

  # binary
  if (n_col <= 2) {
    return(FALSE)
  }

  # multiclass
  if (n_col > 2) {
    return(TRUE)
  }
}

#' @export
is_multiclass.factor <- function(x) {
  lvls <- levels(x)
  n_lvls <- length(lvls)

  if (n_lvls <= 2) {
    return(FALSE)
  }

  if (n_lvls > 2) {
    return(TRUE)
  }
}
