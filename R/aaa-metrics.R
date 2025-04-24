#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance estimates depending
#' on the class of `truth` (see **Value** below) and returns them in a three
#' column tibble. If you wish to modify the metrics used or how they are used
#' see [metric_set()].
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
#' hpc_cv |>
#'   group_by(Resample) |>
#'   metrics(obs, pred, VF:L) |>
#'   print(n = 40)
#'
#' @export metrics
metrics <- function(data, ...) {
  UseMethod("metrics")
}

#' @export
#' @rdname metrics
metrics.data.frame <- function(
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  options = list()
) {
  check_roc_options_deprecated("metrics", options)

  names <- names(data)

  truth <- tidyselect::vars_pull(names, {{ truth }})
  estimate <- tidyselect::vars_pull(names, {{ estimate }})
  probs <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

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
#' If `metric_tweak()` has been used to "tweak" one of these arguments, like
#' `estimator` or `event_level`, then the tweaked version wins. This allows you
#' to set the estimator on a metric by metric basis and still use it in a
#' `metric_set()`.
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
#' hpc_cv |>
#'   group_by(Resample) |>
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
#'     truth = !!rlang::enquo(truth),
#'     estimate = !!rlang::enquo(estimate),
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
#' hpc_cv |>
#'   group_by(Resample) |>
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
  } else if (
    fn_cls %in% c("prob_metric", "class_metric", "ordered_prob_metric")
  ) {
    make_prob_class_metric_function(fns)
  } else if (
    fn_cls %in%
      c(
        "dynamic_survival_metric",
        "static_survival_metric",
        "integrated_survival_metric"
      )
  ) {
    make_survival_metric_function(fns)
  } else {
    # should not be reachable
    cli::cli_abort(
      "{.fn validate_function_class} should have errored on unknown classes.",
      .internal = TRUE
    )
  }
}

#' @export
print.metric_set <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
format.metric_set <- function(x, ...) {
  metrics <- attributes(x)$metrics
  names <- names(metrics)

  cli::cli_format_method({
    cli::cli_text("A metric set, consisting of:")

    metric_formats <- vapply(metrics, format, character(1))
    metric_formats <- strsplit(metric_formats, " | ", fixed = TRUE)

    metric_names <- names(metric_formats)
    metric_types <- vapply(
      metric_formats,
      `[`,
      character(1),
      1,
      USE.NAMES = FALSE
    )
    metric_descs <- vapply(metric_formats, `[`, character(1), 2)
    metric_nchars <- nchar(metric_names) + nchar(metric_types)
    metric_desc_paddings <- max(metric_nchars) - metric_nchars
    # see r-lib/cli#506
    metric_desc_paddings <- lapply(metric_desc_paddings, rep, x = "\u00a0")
    metric_desc_paddings <- vapply(
      metric_desc_paddings,
      paste,
      character(1),
      collapse = ""
    )

    for (i in seq_along(metrics)) {
      cli::cli_text(
        "- {.fun {metric_names[i]}},
           {tolower(metric_types[i])}{metric_desc_paddings[i]} |
           {metric_descs[i]}"
      )
    }
  })
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
    # should not be reachable
    cli::cli_abort(
      "{.code as_label(quo)} resulted in a character vector of length >1.",
      .internal = TRUE
    )
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
  metric_function <- function(
    data,
    truth,
    ...,
    estimate,
    estimator = NULL,
    na_rm = TRUE,
    event_level = yardstick_event_level(),
    case_weights = NULL
  ) {
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
    if (!is_empty(class_fns)) {
      class_args <- quos(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        estimator = estimator,
        na_rm = na_rm,
        event_level = event_level,
        case_weights = !!enquo(case_weights)
      )

      class_calls <- lapply(class_fns, call2, !!!class_args)

      class_calls <- mapply(
        call_remove_static_arguments,
        class_calls,
        class_fns
      )

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
    if (!is_empty(prob_fns)) {
      # TODO - If prob metrics can all do micro, we can remove this
      if (!is.null(estimator) && estimator == "micro") {
        prob_estimator <- NULL
      } else {
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

      prob_calls <- lapply(prob_fns, call2, !!!prob_args)

      prob_calls <- mapply(call_remove_static_arguments, prob_calls, prob_fns)

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
  metric_function <- function(
    data,
    truth,
    estimate,
    na_rm = TRUE,
    case_weights = NULL,
    ...
  ) {
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
    calls <- lapply(fns, call2, !!!call_args)

    calls <- mapply(call_remove_static_arguments, calls, fns)

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
  metric_function <- function(
    data,
    truth,
    ...,
    estimate,
    pred_time,
    na_rm = TRUE,
    case_weights = NULL
  ) {
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
      fns,
      inherits,
      "static_survival_metric",
      FUN.VALUE = logical(1)
    )

    # Construct calls from the functions + arguments
    dynamic_calls <- lapply(fns[!call_class_ind], call2, !!!dynamic_call_args)
    static_calls <- lapply(fns[call_class_ind], call2, !!!static_call_args)

    calls <- c(dynamic_calls, static_calls)

    calls <- mapply(call_remove_static_arguments, calls, fns)

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

validate_not_empty <- function(x, call = caller_env()) {
  if (is_empty(x)) {
    cli::cli_abort(
      "At least 1 function must be supplied to {.code ...}.",
      call = call
    )
  }
}

validate_inputs_are_functions <- function(fns, call = caller_env()) {
  # Check that the user supplied all functions
  is_fun_vec <- vapply(fns, is_function, logical(1))
  all_fns <- all(is_fun_vec)

  if (!all_fns) {
    not_fn <- which(!is_fun_vec)
    cli::cli_abort(
      "All inputs to {.fn metric_set} must be functions.
      These inputs are not: {not_fn}.",
      call = call
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
  valid_cls <- c(
    "class_metric",
    "prob_metric",
    "ordered_prob_metric",
    "numeric_metric",
    "dynamic_survival_metric",
    "static_survival_metric",
    "integrated_survival_metric"
  )

  if (n_unique == 1L) {
    if (fn_cls_unique %in% valid_cls) {
      return(invisible(fns))
    }
  }

  class_prob_cls <- c("class_metric", "prob_metric", "ordered_prob_metric")
  if (
    any(fn_cls_unique %in% class_prob_cls) &&
      all(fn_cls_unique %in% class_prob_cls)
  ) {
    return(invisible(fns))
  }

  surv_cls <- c(
    "dynamic_survival_metric",
    "static_survival_metric",
    "integrated_survival_metric"
  )
  if (any(fn_cls_unique %in% surv_cls) && all(fn_cls_unique %in% surv_cls)) {
    return(invisible(fns))
  }

  # Special case unevaluated groupwise metric factories
  if ("metric_factory" %in% fn_cls) {
    factories <- fn_cls[fn_cls == "metric_factory"]
    cli::cli_abort(
      c(
        "{cli::qty(factories)}The input{?s} {.arg {names(factories)}}
         {?is a/are} {.help [groupwise metric](yardstick::new_groupwise_metric)}
         {?factory/factories} and must be passed a data-column before
         addition to a metric set.",
        "i" = "Did you mean to type e.g. `{names(factories)[1]}(col_name)`?"
      ),
      call = rlang::call2("metric_set")
    )
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
      fn_other_names,
      " ",
      "<",
      env_names_other,
      ">"
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

  cli::cli_abort(
    c(
      "x" = "The combination of metric functions must be:",
      "*" = "only numeric metrics.",
      "*" = "a mix of class metrics and class probability metrics.",
      "*" = "a mix of dynamic and static survival metrics.",
      "i" = "The following metric function types are being mixed:",
      fn_pastable
    ),
    call = rlang::call2("metric_set")
  )
}

# Safely evaluate metrics in such a way that we can capture the
# error and inform the user of the metric that failed
eval_safely <- function(expr, expr_nm, data = NULL, env = caller_env()) {
  tryCatch(
    expr = {
      eval_tidy(expr, data = data, env = env)
    },
    error = function(cnd) {
      cli::cli_abort(
        "Failed to compute {.fn {expr_nm}}.",
        parent = cnd,
        call = call("metric_set")
      )
    }
  )
}

call_remove_static_arguments <- function(call, fn) {
  static <- get_static_arguments(fn)

  if (length(static) == 0L) {
    # No static arguments
    return(call)
  }

  names <- rlang::call_args_names(call)
  names <- intersect(names, static)

  if (length(names) == 0L) {
    # `static` arguments don't intersect with `call`
    return(call)
  }

  zaps <- rlang::rep_named(names, list(rlang::zap()))
  call <- call_modify(call, !!!zaps)

  call
}
