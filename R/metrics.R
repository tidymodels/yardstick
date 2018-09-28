#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance
#'  estimates depending on the class of `truth` (see **Value**
#'  below) and returns them in a two column tibble.
#'
#'
#' @inheritParams roc_auc
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns and any columns specified by `...`.
#' @param truth The column identifier for the true results (that
#'  is `numeric` or `factor`). This should be an unquoted column name
#'  although this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names).
#' @param estimate The column identifier for the predicted results
#'  (that is also `numeric` or `factor`). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#'
#' @return A two column tibble.
#' * When `truth` is a factor, there are columns for [accuracy()] and the
#' Kappa statistic ([kap()]).
#' * If a full set of class probability columns are passed to `...`, then
#' there is also a column for [mnLogLoss()].
#' * When `truth` has two levels and there are class probabilities, [roc_auc()]
#' is appended.
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
#' metrics(two_class_example, truth, predicted, Class1, Class2)
#'
#' # Regression metrics
#' metrics(solubility_test, truth = solubility, estimate = prediction)
#'

#' @export metrics
metrics <- function(data, ...)
  UseMethod("metrics")

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

    # Precompute the table for speed
    xtab <- vec2table(
      truth = data[[ vars$truth ]],
      estimate = data[[ vars$estimate ]],
      na.rm = na.rm,
      dnn = c("Prediction", "Truth")
    )

    metrics_class <- metric_set(accuracy, kap)

    res <- metrics_class(xtab)

    # truth=factor. Any ... ?
    has_probs <- !all(is.na(vars$probs))

    if (has_probs) {

      res <- bind_rows(
        res,
        mnLogLoss(data, !! vars$truth, !! vars$probs, na.rm = na.rm)
      )

      # truth=factor and there are ...
      # Is truth a 2 level factor?
      lvl <- levels(data[[ vars$truth ]])

      if (length(lvl) == 2) {

        col <- if (getOption("yardstick.event_first"))
          lvl[1]
        else
          lvl[2]

        res <- bind_rows(
          res,
          roc_auc(data, !! vars$truth, !! col, na.rm = na.rm, options = options)
        )

      } # end two_classes

    } # end has_probs

    # truth != factor
  } else {

    # Assume only regression for now
    if(!is.numeric(data[[ vars$estimate ]]))
      stop("`estimate` should be numeric", call. = FALSE)

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


#' Combine metric functions
#'
#' `metric_set()` allows you to combine multiple metric functions together
#' into a new function that calculates all of them at once.
#'
#' @param ... The bare names of the functions to be included in the metric set.
#'
#' @details
#'
#' There are currently no checks in place to ensure that all of the metric
#' functions calculate the same kind of metric (regression vs classification).
#'
#' @examples
#'
#' library(dplyr)
#'
#' # Multiple regression metrics
#' multi_metric <- metric_set(rmse, rsq, ccc)
#'
#' multi_metric(solubility_test, truth = solubility, estimate = prediction)
#'
#' # Groups are respected on the new metric function
#' class_metrics <- metric_set(accuracy, kap)
#'
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   class_metrics(obs, pred)
#'
#' # If you need to set options for certain metrics,
#' # do so ahead of time with purrr::partial()
#' library(purrr)
#' ccc_with_bias <- partial(ccc, bias = TRUE)
#'
#' multi_metric2 <- metric_set(rmse, rsq, ccc_with_bias)
#'
#' multi_metric2(solubility_test, truth = solubility, estimate = prediction)
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

  # Capture functions in their environment
  quo_fns <- enquos(...)
  fns <- lapply(quo_fns, eval_tidy)

  validate_inputs_are_functions(fns)

  names(fns) <- vapply(quo_fns, quo_name, character(1))

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
      stop("In metric: `", expr_nm, "`\n", e$message, call. = FALSE)
    }
  )
}
