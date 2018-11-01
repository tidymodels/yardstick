#' Sensitivity
#'
#' These functions calculate the [sens()] (sensitivity) of a measurement system
#' compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [spec()], [ppv()], and [npv()].
#'
#' The sensitivity (`sens()`) is defined as the proportion of positive
#' results out of the number of samples which were actually
#' positive. When there are no positive results, sensitivity is not
#' defined and a value of `NA` is returned.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar metric_fn sens
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @param data Either a `data.frame` containing the `truth` and `estimate`
#' columns, or a `table`/`matrix` where the true class results should be
#' in the columns of the table.
#'
#' @param truth The column identifier for the true class results
#'  (that is a `factor`). This should be an unquoted column name although
#'  this argument is passed by expression and supports
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names). For `_vec()` functions, a `factor` vector.
#'
#' @param estimate The column identifier for the predicted class
#'  results (that is also `factor`). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name. For `_vec()` functions, a `factor` vector.
#'
#' @param estimator One of: `"binary"`, `"macro"`, `"macro_weighted"`,
#' or `"micro"` to specify the type of averaging to be done. `"binary"` is
#' only relevant for the two class case. The other three are general methods for
#' calculating multiclass metrics. The default will automatically choose `"binary"`
#' or `"macro"` based on `estimate`.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#' sensitivity and specificity,'' *British Medical Journal*,
#' vol 308, 1552.
#'
#' @template examples-class
#'
#' @export
#'
sens <- function(data, ...) {
  UseMethod("sens")
}

class(sens) <- c("class_metric", "function")

#' @export
#' @rdname sens
sens.data.frame <- function(data, truth, estimate,
                            estimator = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "sens",
    metric_fn = sens_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
sens.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "sens",
    .estimator = estimator,
    .estimate = sens_table_impl(data, estimator)
  )

}

#' @export
sens.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  sens.table(data, estimator)

}

#' @export
#' @rdname sens
sens_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  sens_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    sens_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = sens_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

# sensitivity = recall
sens_table_impl <- recall_table_impl
