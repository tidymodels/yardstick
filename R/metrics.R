#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance
#'  estimates depending on the class of `truth` (see **Value**
#'  below) and returns them in a three column tibble.
#'
#' @inheritParams roc_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth`,
#' `estimate`, and `...`.
#'
#' @param truth The column identifier for the true results (that
#' is `numeric` or `factor`). This should be an unquoted column name
#' although this argument is passed by expression and support
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names).
#'
#' @param estimate The column identifier for the predicted results
#' (that is also `numeric` or `factor`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name.
#'
#' @return
#'
#' A three column tibble.
#'
#' * When `truth` is a factor, there are rows for [accuracy()] and the
#' Kappa statistic ([kap()]).
#'
#' * When `truth` has two levels and 1 column of class probabilities is
#' passed to `...`, there are rows for the two class versions of
#' [mn_log_loss()] and [roc_auc()].
#'
#' * When `truth` has more than two levels and a full set of class probabilities
#' are passed to `...`, there are rows for the multiclass version of
#' [mn_log_loss()] and the Hand Till generalization of [roc_auc()].
#'
#' * When `truth` is numeric, there are rows for [rmse()], [rsq()],
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
#' # for roc_auc() besides the default, hand_till. Use the specific function
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
metrics.data.frame <- function(data,
                               truth,
                               estimate,
                               ...,
                               na_rm = TRUE,
                               options = list()) {
  check_roc_options_deprecated("metrics", options)

  names <- names(data)

  truth <- tidyselect::vars_pull(names, {{truth}})
  estimate <- tidyselect::vars_pull(names, {{estimate}})
  probs <- unname(tidyselect::vars_select(names, ...))

  if (is_class_pred(data[[truth]])) {
    data[[truth]] <- as_factor_from_class_pred(data[[truth]])
  }
  if (is_class_pred(data[[estimate]])) {
    data[[estimate]] <- as_factor_from_class_pred(data[[estimate]])
  }

  is_class <- is.factor(data[[truth]]) || is_class_pred(data[[truth]])

  if (is_class) {
    metrics_class <- metric_set(accuracy, kap)
    res <- metrics_class(data, !!truth, estimate = !!estimate, na_rm = na_rm)

    if (length(probs) > 0L) {
      res2 <- mn_log_loss(data, !!truth, !!probs, na_rm = na_rm)
      res3 <- roc_auc(data, !!truth, !!probs, na_rm = na_rm)
      res <- dplyr::bind_rows(res, res2, res3)
    }
  } else {
    # Assume only regression for now
    metrics_regression <- metric_set(rmse, rsq, mae)

    res <- metrics_regression(
      data = data,
      truth = !!truth,
      estimate = !!estimate,
      na_rm = na_rm
    )
  }

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
#' All functions must be either:
#' - Only numeric metrics
#' - A mix of class metrics or class prob metrics
#' - A mix of dynamic, integrated, and static survival metrics
#'
#' For instance, `rmse()` can be used with `mae()` because they
#' are numeric metrics, but not with `accuracy()` because it is a classification
#' metric. But `accuracy()` can be used with `roc_auc()`.
#'
#' The returned metric function will have a different argument list
#' depending on whether numeric metrics or a mix of class/prob metrics were
#' passed in.
#'
#' ```
#' # Numeric metric set signature:
#' fn(
#'   data,
#'   truth,
#'   estimate,
#'   na_rm = TRUE,
#'   case_weights = NULL,
#'   ...
#' )
#'
#' # Class / prob metric set signature:
#' fn(
#'   data,
#'   truth,
#'   ...,
#'   estimate,
#'   estimator = NULL,
#'   na_rm = TRUE,
#'   event_level = yardstick_event_level(),
#'   case_weights = NULL
#' )
#'
#' # Dynamic / integrated / static survival metric set signature:
#' fn(
#'   data,
#'   truth,
#'   ...,
#'   estimate,
#'   na_rm = TRUE,
#'   case_weights = NULL
#' )
#' ```
#'
#' When mixing class and class prob metrics, pass in the hard predictions
#' (the factor column) as the named argument `estimate`, and the soft
#' predictions (the class probability columns) as bare column names or
#' `tidyselect` selectors to `...`.
#'
#' When mixing dynamic, integrated, and static survival metrics, pass in the
#' time predictions as the named argument `estimate`, and the survival
#' predictions as bare column names or `tidyselect` selectors to `...`.
#'
#' @examples
#' library(dplyr)
#'
#' # Multiple regression metrics
#' multi_metric <- metric_set(rmse, rsq, ccc)
#'
#' # The returned function has arguments:
#' # fn(data, truth, estimate, na_rm = TRUE, ...)
#' multi_metric(solubility_test, truth = solubility, estimate = prediction)
#'
#' # Groups are respected on the new metric function
#' class_metrics <- metric_set(accuracy, kap)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   class_metrics(obs, estimate = pred)
#'
#' # ---------------------------------------------------------------------------
#'
#' # If you need to set options for certain metrics,
#' # do so by wrapping the metric and setting the options inside the wrapper,
#' # passing along truth and estimate as quoted arguments.
#' # Then add on the function class of the underlying wrapped function,
#' # and the direction of optimization.
#' ccc_with_bias <- function(data, truth, estimate, na_rm = TRUE, ...) {
#'   ccc(
#'     data = data,
#'     truth = !! rlang::enquo(truth),
#'     estimate = !! rlang::enquo(estimate),
#'     # set bias = TRUE
#'     bias = TRUE,
#'     na_rm = na_rm,
#'     ...
#'   )
#' }
#'
#' # Use `new_numeric_metric()` to formalize this new metric function
#' ccc_with_bias <- new_numeric_metric(ccc_with_bias, "maximize")
#'
#' multi_metric2 <- metric_set(rmse, rsq, ccc_with_bias)
#'
#' multi_metric2(solubility_test, truth = solubility, estimate = prediction)
#'
#' # ---------------------------------------------------------------------------
#' # A class probability example:
#'
#' # Note that, when given class or class prob functions,
#' # metric_set() returns a function with signature:
#' # fn(data, truth, ..., estimate)
#' # to be able to mix class and class prob metrics.
#'
#' # You must provide the `estimate` column by explicitly naming
#' # the argument
#'
#' class_and_probs_metrics <- metric_set(roc_auc, pr_auc, accuracy)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   class_and_probs_metrics(obs, VF:L, estimate = pred)
#'
#' @seealso [metrics()]
#'
#' @export
metric_set <- function(...) {
  quo_fns <- enquos(...)
  validate_not_empty(quo_fns)

  # Get values and check that they are fns
  fns <- lapply(quo_fns, eval_tidy)
  validate_inputs_are_functions(fns)

  # Add on names, and then check that
  # all fns are of the same function class
  names(fns) <- vapply(quo_fns, get_quo_label, character(1))
  validate_function_class(fns)

  fn_cls <- class1(fns[[1]])

  # signature of the function is different depending on input functions
  if (fn_cls == "numeric_metric") {
    make_numeric_metric_function(fns)
  } else if(fn_cls %in% c("prob_metric", "class_metric")) {
    make_prob_class_metric_function(fns)
  } else if(fn_cls %in% c("dynamic_survival_metric",
                          "static_survival_metric",
                          "integrated_survival_metric")) {
    make_survival_metric_function(fns)
  } else {
    abort(paste0(
      "Internal error: `validate_function_class()` should have ",
      "errored on unknown classes."
    ))
  }
}

#' @export
print.metric_set <- function(x, ...) {
  info <- dplyr::as_tibble(x)
  print(info)
  invisible(x)
}

#' @export
as_tibble.metric_set <- function(x, ...) {
  metrics <- attributes(x)$metrics
  names <- names(metrics)
  metrics <- unname(metrics)

  classes <- map_chr(metrics, class1)
  directions <- map_chr(metrics, get_metric_fn_direction)

  dplyr::tibble(
    metric = names,
    class = classes,
    direction = directions
  )
}

map_chr <- function(x, f, ...) {
  vapply(x, f, character(1), ...)
}

class1 <- function(x) {
  class(x)[[1]]
}

get_metric_fn_direction <- function(x) {
  attr(x, "direction")
}

get_quo_label <- function(quo) {
  out <- as_label(quo)

  if (length(out) != 1L) {
    abort("Internal error: `as_label(quo)` resulted in a character vector of length >1.")
  }

  is_namespaced <- grepl("::", out, fixed = TRUE)

  if (is_namespaced) {
    # Split by `::` and take the second half
    split <- strsplit(out, "::", fixed = TRUE)[[1]]
    out <- split[[2]]
  }

  out
}

make_prob_class_metric_function <- function(fns) {
  metric_function <- function(data,
                              truth,
                              ...,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = yardstick_event_level(),
                              case_weights = NULL) {

    # Find class vs prob metrics
    are_class_metrics <- vapply(
      X = fns,
      FUN = inherits,
      FUN.VALUE = logical(1),
      what = "class_metric"
    )

    class_fns <- fns[are_class_metrics]
    prob_fns <- fns[!are_class_metrics]

    metric_list <- list()

    # Evaluate class metrics
    if(!is_empty(class_fns)) {

      class_args <- quos(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        estimator = estimator,
        na_rm = na_rm,
        event_level = event_level,
        case_weights = !!enquo(case_weights)
      )

      class_calls <- lapply(class_fns, call2, !!! class_args)

      class_calls <- lapply(class_calls, call_remove_duplicate_arguments)

      class_list <- mapply(
        FUN = eval_safely,
        class_calls, # .x
        names(class_calls), # .y
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      metric_list <- c(metric_list, class_list)
    }

    # Evaluate prob metrics
    if(!is_empty(prob_fns)) {

      # TODO - If prob metrics can all do micro, we can remove this
      if (!is.null(estimator) && estimator == "micro") {
        prob_estimator <- NULL
      }
      else {
        prob_estimator <- estimator
      }

      prob_args <- quos(
        data = data,
        truth = !!enquo(truth),
        ... = ...,
        estimator = prob_estimator,
        na_rm = na_rm,
        event_level = event_level,
        case_weights = !!enquo(case_weights)
      )

      prob_calls <- lapply(prob_fns, call2, !!! prob_args)

      prob_calls <- lapply(prob_calls, call_remove_duplicate_arguments)

      prob_list <- mapply(
        FUN = eval_safely,
        prob_calls, # .x
        names(prob_calls), # .y
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      metric_list <- c(metric_list, prob_list)
    }

    dplyr::bind_rows(metric_list)
  }

  class(metric_function) <- c(
    "class_prob_metric_set",
    "metric_set",
    class(metric_function)
  )

  attr(metric_function, "metrics") <- fns

  metric_function
}

make_numeric_metric_function <- function(fns) {
  metric_function <- function(data,
                              truth,
                              estimate,
                              na_rm = TRUE,
                              case_weights = NULL,
                              ...) {

    # Construct common argument set for each metric call
    # Doing this dynamically inside the generated function means
    # we capture the correct arguments
    call_args <- quos(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm,
      case_weights = !!enquo(case_weights),
      ... = ...
    )

    # Construct calls from the functions + arguments
    calls <- lapply(fns, call2, !!! call_args)

    calls <- lapply(calls, call_remove_duplicate_arguments)

    # Evaluate
    metric_list <- mapply(
      FUN = eval_safely,
      calls, # .x
      names(calls), # .y
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    dplyr::bind_rows(metric_list)
  }

  class(metric_function) <- c(
    "numeric_metric_set",
    "metric_set",
    class(metric_function)
  )

  attr(metric_function, "metrics") <- fns

  metric_function
}

make_survival_metric_function <- function(fns) {
  metric_function <- function(data,
                              truth,
                              ...,
                              estimate,
                              pred_time,
                              na_rm = TRUE,
                              case_weights = NULL) {
    # Construct common argument set for each metric call
    # Doing this dynamically inside the generated function means
    # we capture the correct arguments
    dynamic_call_args <- quos(
      data = data,
      truth = !!enquo(truth),
      ... = ...,
      na_rm = na_rm,
      case_weights = !!enquo(case_weights),
      ... = ...
    )

    static_call_args <- quos(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm,
      case_weights = !!enquo(case_weights),
      ... = ...
    )

    call_class_ind <- vapply(
      fns, inherits, "static_survival_metric", FUN.VALUE = logical(1)
    )

    # Construct calls from the functions + arguments
    dynamic_calls <- lapply(fns[!call_class_ind], call2, !!! dynamic_call_args)
    static_calls <- lapply(fns[call_class_ind], call2, !!! static_call_args)

    calls <- c(dynamic_calls, static_calls)

    # Evaluate
    metric_list <- mapply(
      FUN = eval_safely,
      calls, # .x
      names(calls), # .y
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    dplyr::bind_rows(metric_list)
  }

  class(metric_function) <- c(
    "survival_metric_set",
    "metric_set",
    class(metric_function)
  )

  attr(metric_function, "metrics") <- fns

  metric_function
}

validate_not_empty <- function(x) {
  if (is_empty(x)) {
    abort("`metric_set()` requires at least 1 function supplied to `...`.")
  }
}

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

# Validate that all metric functions inherit from valid function classes or
# combinations of classes
validate_function_class <- function(fns) {
  fn_cls <- vapply(fns, function(fn) class(fn)[1], character(1))
  fn_cls_unique <- unique(fn_cls)
  n_unique <- length(fn_cls_unique)

  if (n_unique == 0L) {
    return(invisible(fns))
  }
  valid_cls <- c("class_metric", "prob_metric", "numeric_metric",
                 "dynamic_survival_metric", "static_survival_metric",
                 "integrated_survival_metric")

  if (n_unique == 1L) {
    if (fn_cls_unique %in% valid_cls) {
      return(invisible(fns))
    }
  }

  # Special case of ONLY class and prob functions together
  # These are allowed to be together
  if (n_unique == 2) {
    if (fn_cls_unique[1] %in% c("class_metric", "prob_metric") &&
        fn_cls_unique[2] %in% c("class_metric", "prob_metric")) {
      return(invisible(fns))
    }

    if (fn_cls_unique[1] %in% c("dynamic_survival_metric",
                                "static_survival_metric",
                                "integrated_survival_metric") &&
        fn_cls_unique[2] %in% c("dynamic_survival_metric",
                                "static_survival_metric",
                                "integrated_survival_metric")) {
      return(invisible(fns))
    }
  }

  if (n_unique == 3) {
    if (fn_cls_unique[1] %in% c("dynamic_survival_metric",
                                "static_survival_metric",
                                "integrated_survival_metric") &&
        fn_cls_unique[2] %in% c("dynamic_survival_metric",
                                "static_survival_metric",
                                "integrated_survival_metric") &&
        fn_cls_unique[3] %in% c("dynamic_survival_metric",
                                "static_survival_metric",
                                "integrated_survival_metric")) {
      return(invisible(fns))
    }
  }

  # Each element of the list contains the names of the fns
  # that inherit that specific class
  fn_bad_names <- lapply(fn_cls_unique, function(x) {
    names(fns)[fn_cls == x]
  })

  # clean up for nicer printing
  fn_cls_unique <- gsub("_metric", "", fn_cls_unique)
  fn_cls_unique <- gsub("function", "other", fn_cls_unique)

  fn_cls_other <- fn_cls_unique == "other"

  if (any(fn_cls_other)) {
    fn_cls_other_loc <- which(fn_cls_other)
    fn_other_names <- fn_bad_names[[fn_cls_other_loc]]
    fns_other <- fns[fn_other_names]

    env_names_other <- vapply(
      fns_other,
      function(fn) env_name(fn_env(fn)),
      character(1)
    )

    fn_bad_names[[fn_cls_other_loc]] <- paste0(
      fn_other_names, " ", "<", env_names_other, ">"
    )
  }

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
    "\nThe combination of metric functions must be:\n",
    "- only numeric metrics\n",
    "- a mix of class metrics and class probability metrics\n\n",
    "- a mix of dynamic and static survival metrics\n\n",
    "The following metric function types are being mixed:\n",
    fn_pastable
  ))
}

# Safely evaluate metrics in such a way that we can capture the
# error and inform the user of the metric that failed
eval_safely <- function(expr, expr_nm, data = NULL, env = caller_env()) {
  tryCatch(
    expr = {
      eval_tidy(expr, data = data, env = env)
    },
    error = function(cnd) {
      message <- paste0("Failed to compute `", expr_nm, "()`.")
      abort(message, parent = cnd, call = call("metric_set"))
    }
  )
}


call_remove_duplicate_arguments <- function(call) {
  arg_names <- environment(call[[1]])[["fixed"]]

  newargs <- rlang::rep_named(names(arg_names), list(rlang::zap()))

  call <- rlang::call_modify(call, !!!newargs)
  call
}
