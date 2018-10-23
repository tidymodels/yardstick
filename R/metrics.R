#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance
#'  estimates depending on the class of `truth` (see **Value**
#'  below) and returns them in a two column tibble.
#'
#'
#' @inheritParams roc_auc
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns and any columns specified by `...`.
#'
#' @param truth The column identifier for the true results (that
#'  is `numeric` or `factor`). This should be an unquoted column name
#'  although this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names).
#'
#' @param estimate The column identifier for the predicted results
#'  (that is also `numeric` or `factor`). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#'
#' @return A two column tibble.
#' * When `truth` is a factor, there are columns for [accuracy()] and the
#' Kappa statistic ([kap()]).
#' * When `truth` has two levels, and 1 column of class probabilities is
#' passed to `...`, there are columns for the two class versions of
#' [mn_log_loss()] and [roc_auc()].
#' * When `truth` has more than two levels, and a full set of class probabilities
#' are passed to `...`, there are columns for the multiclass version of
#' [mn_log_loss()] and macro averaged [roc_auc()].
#' * When `truth` is numeric, there are columns for [rmse()], [rsq()],
#' and [mae()].
#'
#' @seealso [metric_set()]
#'
#' @examples
#'
#' # Accuracy and kappa
#' metrics(two_class_example, truth, predicted)
#'
#' # Add on multinomal log loss and ROC AUC by specifying class prob columns
#' metrics(two_class_example, truth, predicted, Class1)
#'
#' # Regression metrics
#' metrics(solubility_test, truth = solubility, estimate = prediction)
#'
#' # Multiclass metrics work, but you cannot specify any averaging
#' # for roc_auc() besides the default, macro. Use the specific function
#' # if you need more customization
#' library(dplyr)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   metrics(obs, pred, VF:L) %>%
#'   print(n = 40)
#'
#' @export metrics
metrics <- function(data, ...) {
  UseMethod("metrics")
}

#' @export
#' @rdname metrics
#' @importFrom dplyr bind_rows
metrics.data.frame <- function(data, truth, estimate, ...,
                               options = list(), na.rm = TRUE) {

  # Get set of character vars
  vars <- all_select(
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    ...
  )

  is_class <- is.factor(data[[ vars$truth ]])

  if (is_class) {

    if(!is.factor(data[[ vars$estimate ]])) {
      stop("`estimate` should be a factor", call. = FALSE)
    }

    metrics_class <- metric_set(accuracy, kap)

    res <- metrics_class(data, !! vars$truth, !!vars$estimate)

    # truth=factor. Any ... ?
    has_probs <- !all(is.na(vars$probs))

    if (has_probs) {

      # truth=factor and there are ...
      # Is truth a 2 level factor?
      lvl <- levels(data[[ vars$truth ]])

      res <- bind_rows(
        res,
        mn_log_loss(data, !! vars$truth, !! vars$probs, na.rm = na.rm),
        roc_auc(data, !! vars$truth, !! vars$probs, na.rm = na.rm, options = options)
      )

    } # end has_probs

    # truth != factor
  } else {

    # Assume only regression for now
    if (!is.numeric(data[[ vars$estimate ]])) {
      stop("`estimate` should be numeric", call. = FALSE)
    }

    metrics_regression <- metric_set(rmse, rsq, mae)

    res <- metrics_regression(
      data = data,
      truth = !! vars$truth,
      estimate = !! vars$estimate,
      na.rm = na.rm
    )

  } # end regression

  res
}

# Metric set -------------------------------------------------------------------

#' Combine metric functions
#'
#' `metric_set()` allows you to combine multiple metric functions together
#' into a new function that calculates all of them at once.
#'
#' @param ... The bare names of the functions to be included in the metric set.
#'
#' @details
#'
#' All functions must be of the same "function class" to be able to be used
#' together. For instance, `rmse()` can be used with `mae()` because they
#' are numeric metrics, but not with `accuracy()` because it is a classification
#' metric.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # Multiple regression metrics
#' multi_metric <- metric_set(rmse, rsq, ccc)
#'
#' # The returned function has arguments:
#' # fn(data, truth, estimate, na.rm = TRUE, ...)
#' multi_metric(solubility_test, truth = solubility, estimate = prediction)
#'
#' # Groups are respected on the new metric function
#' class_metrics <- metric_set(accuracy, kap)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   class_metrics(obs, pred)
#'
#' # ---------------------------------------------------------------------------
#'
#' # If you need to set options for certain metrics,
#' # do so ahead of time with purrr::partial(). Unfortunately
#' # this requires you to reset the class of the metric function manually
#' # as partial() removes it. Metric function classes are one of:
#' # "numeric_metric", "class_metric", "prob_metric"
#' library(purrr)
#' ccc_with_bias <- partial(ccc, bias = TRUE)
#' class(ccc_with_bias) <- c("numeric_metric", "function")
#'
#' multi_metric2 <- metric_set(rmse, rsq, ccc_with_bias)
#'
#' multi_metric2(solubility_test, truth = solubility, estimate = prediction)
#'
#' # ---------------------------------------------------------------------------
#' # A class probability example:
#'
#' # Note that, when given prob functions, metric_set() returns a function
#' # with signature:
#' # fn(data, truth, ...)
#' # to be consistent with class probability metric functions
#'
#' probs_metrics <- metric_set(roc_auc, pr_auc, mn_log_loss)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   probs_metrics(obs, VF:L)
#'
#' @seealso [metrics()]
#'
#' @export
#'
#' @importFrom rlang call2
#' @importFrom dplyr bind_rows
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
metric_set <- function(...) {

  quo_fns <- enquos(...)
  validate_not_empty(quo_fns)

  # Get values and check that they are fns
  fns <- lapply(quo_fns, eval_tidy)
  validate_inputs_are_functions(fns)

  # Add on names, and then check that
  # all fns are of the same function class
  names(fns) <- vapply(quo_fns, quo_name, character(1))
  validate_function_class(fns)

  fn_cls <- class(fns[[1]])[1]

  # signature of the function is different depending on input functions
  if (fn_cls %in% c("class_metric", "numeric_metric")) {

    function(data, truth, estimate, na.rm = TRUE, ...) {

      # Construct common argument set for each metric call
      # Doing this dynamically inside the generated function means
      # we capture the correct arguments
      call_args <- quos(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        na.rm = na.rm,
        ... = ...
      )

      # Construct calls from the functions + arguments
      calls <- lapply(fns, call2, !!! call_args)

      # Evaluate
      metric_list <- mapply(
        FUN = eval_safely,
        calls, # .x
        names(calls), # .y
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      bind_rows(metric_list)
    }

  }
  else if (fn_cls == "prob_metric") {

    function(data, truth, ..., na.rm = TRUE) {

      # Construct common argument set for each metric call
      # Doing this dynamically inside the generated function means
      # we capture the correct arguments
      call_args <- quos(
        data = data,
        truth = !!enquo(truth),
        ... = ...,
        na.rm = na.rm
      )

      # Construct calls from the functions + arguments
      calls <- lapply(fns, call2, !!! call_args)

      # Evaluate
      metric_list <- mapply(
        FUN = eval_safely,
        calls, # .x
        names(calls), # .y
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      bind_rows(metric_list)
    }

  }
  else {
    abort(paste0(
      "No `metric_set()` implementation available for functions of class: ",
      fn_cls, "."
    ))
  }

}


validate_not_empty <- function(x) {
  if (rlang::is_empty(x)) {
    abort("`metric_set()` requires at least 1 function supplied to `...`.")
  }
}

#' @importFrom rlang is_function
validate_inputs_are_functions <- function(fns) {

  # Check that the user supplied all functions
  is_fun_vec <- vapply(fns, is_function, logical(1))
  all_fns <- all(is_fun_vec)

  if(!all_fns) {
    not_fn <- which(!is_fun_vec)
    not_fn <- paste(not_fn, collapse = ", ")
    stop(
      "All inputs to `metric_set()` must be functions. ",
      "These inputs are not: (", not_fn, ").",
      call. = FALSE
    )
  }

}

# Validate that all metric functions inherit from the same function class
validate_function_class <- function(fns) {
  fn_cls <- vapply(fns, function(fn) class(fn)[1], character(1))
  fn_cls_unique <- unique(fn_cls)

  if (length(fn_cls_unique) > 1) {

    # Each element of the list contains the names of the fns
    # that inherit that specific class
    fn_bad_names <- lapply(fn_cls_unique, function(x) {
      x <- unique(c(x, "function"))
      fn_nms <- names(fns)
      where <- vapply(fns, rlang::inherits_only, logical(1), class = x)
      fn_nms[where]
    })

    # clean up for nicer printing
    fn_cls_unique <- gsub("_metric", "", fn_cls_unique)
    fn_cls_unique <- gsub("function", "other", fn_cls_unique)

    # Prints as:
    # - fn_type1 (fn_name1, fn_name2)
    # - fn_type2 (fn_name1)
    fn_pastable <- mapply(
      FUN = function(fn_type, fn_names) {
        fn_names <- paste0(fn_names, collapse = ", ")
        paste0("- ", fn_type, " (", fn_names, ")")
      },
      fn_type = fn_cls_unique,
      fn_names = fn_bad_names,
      USE.NAMES = FALSE
    )

    fn_pastable <- paste0(fn_pastable, collapse = "\n")

    abort(paste0(
      "All metric functions must be of the same class. ",
      "The following metric function types are being mixed:\n",
      fn_pastable
    ))
  }
}

# Safely evaluate metrics in such a way that we can capture the
# error and inform the user of the metric that failed

#' @importFrom rlang caller_env
#' @importFrom rlang eval_tidy
eval_safely <- function(expr, expr_nm, data = NULL, env = caller_env()) {
  tryCatch(
    expr = {
      eval_tidy(expr, data = data, env = env)
    },
    error = function(e) {
      abort(paste0(
        "In metric: `", expr_nm, "`\n",
        e$message
      ))
    }
  )
}
