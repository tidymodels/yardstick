#' Calculate sensitivity, specificity and predictive values
#'
#' These functions calculate the sensitivity, specificity or
#'  predictive values of a measurement system compared to a
#'  reference results (the truth or a gold standard). The
#'  measurement and "truth" data must have the same two possible
#'  outcomes and one of the outcomes must be thought of as a
#'  "positive" results or the "event".
#'
#' The sensitivity (`sens()`) is defined as the proportion of positive
#'  results out of the number of samples which were actually
#'  positive. When there are no positive results, sensitivity is not
#'  defined and a value of `NA` is returned. Similarly, when
#'  there are no negative results, specificity (`spec()`) is not defined and a
#'  value of `NA` is returned. Similar statements are true for
#'  predictive values.
#'
#' The positive predictive value (`ppv()`) is defined as the percent of
#'  predicted positives that are actually positive while the
#'  negative predictive value (`npv()`) is defined as the percent of negative
#'  positives that are actually negative.
#'
#' There is no common convention on which factor level should
#'  automatically be considered the "event" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Event \tab No Event
#' \cr Event \tab A \tab B \cr No Event \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{Sensitivity = A/(A+C)} \deqn{Specificity =
#' D/(B+D)} \deqn{Prevalence = (A+C)/(A+B+C+D)} \deqn{PPV = (sensitivity *
#' Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))}
#' \deqn{NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) +
#' ((specificity)*(1-Prevalence)))}
#'
#' See the references for discussions of the statistics.
#'
#' If more than one statistic is required, it is more
#'  computationally efficient to create the confusion matrix using
#'  [conf_mat()] and applying the corresponding `summary` method
#'  ([summary.conf_mat()]) to get the values at once.
#'
#' @aliases sens sens.default sens.table sens.matrix spec
#'  spec.default spec.table spec.matrix ppv ppv.default ppv.table
#'  ppv.matrix npv npv.default npv.table npv.matrix
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
#' @param averaging One of: `"binary"`, `"macro"`, `"macro_weighted"`,
#' or `"micro"` to specify the type of averaging to be done. `"binary"` is
#' only relevant for the two class case. The other three are general methods for
#' calculating multiclass metrics. The default will automatically choose `"binary"`
#' or `"macro"` based on `estimate`.
#'
#' @param prevalence A numeric value for the rate of the
#'  "positive" class of the data.
#'
#' @param na.rm A `logical` value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @return For `_vec()` functions, a single `numeric` value (or `NA`).
#' Otherwise, a `tibble` with columns `.metric` and `.estimate` and 1 row of
#' values. For grouped data frames, the number of rows returned will be the
#' same as the number of groups.
#'
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [mcc()]
#'
#' @references Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#'  sensitivity and specificity,'' *British Medical Journal*,
#'  vol 308, 1552.
#'
#'   Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2:
#'  predictive values,'' *British Medical Journal*, vol 309,
#'  102.
#'
#' @keywords manip
#'
#' @examples
#' data("two_class_example")
#'
#' # Given that a sample is Class 1,
#' #   what is the probability that is predicted as Class 1?
#' sens(two_class_example, truth = truth, estimate = predicted)
#'
#' # Given that a sample is predicted to be Class 1,
#' #  what is the probability that it truly is Class 1?
#' ppv(two_class_example, truth = truth, estimate = predicted)
#'
#' # But what if we think that Class 1 only occurs 40% of the time?
#' ppv(two_class_example, truth, predicted, prevalence = 0.40)
#'
#' # Vector arguments can be used with _vec() functions
#' sens_vec(two_class_example$truth, two_class_example$predicted)
#'
#' @export sens
sens <- function(data, ...)
  UseMethod("sens")

#' @export
#' @rdname sens
sens.data.frame <- function(data, truth, estimate, averaging = "binary", na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = construct_name("sens", averaging),
    metric_fn = sens_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname sens
#' @export
sens.table <- function(data, averaging = "binary", ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("sens", averaging),
    .estimate = sens_table_impl(data, averaging)
  )

}

#' @export
#' @rdname sens
sens.matrix <- function(data, averaging = "binary", ...) {

  data <- as.table(data)
  sens.table(data, averaging)

}

#' @export
#' @rdname sens
sens_vec <- function(truth, estimate, averaging = "binary", na.rm = TRUE, ...) {

  sens_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    sens_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = sens_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

# sensitivity = recall
sens_table_impl <- recall_table_impl

#' @export
spec <-  function(data, ...) {
  UseMethod("spec")
}


#' @export
#' @rdname sens
spec.data.frame <- function(data, truth, estimate, averaging = "binary",
                            na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = construct_name("spec", averaging),
    metric_fn = spec_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname sens
spec.table <- function(data, averaging = "binary", ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("spec", averaging),
    .estimate = spec_table_impl(data, averaging)
  )

}

#' @rdname sens
#' @export
spec.matrix <- function(data, averaging = "binary", ...) {

  data <- as.table(data)
  spec.table(data, averaging)

}

#' @export
#' @rdname sens
spec_vec <- function(truth, estimate, averaging = "binary", na.rm = TRUE, ...) {

  spec_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    spec_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = spec_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

spec_table_impl <- function(data, averaging) {

  if(is_binary(averaging)) {
    spec_binary(data)
  } else {
    w <- get_weights(data, averaging)
    out_vec <- spec_multiclass(data, averaging)
    weighted.mean(out_vec, w)
  }

}

spec_binary <- function(data) {

  negative <- neg_val(data)

  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])
  spec <- ifelse(denom > 0, numer / denom, NA_real_)
  spec

}

spec_multiclass <- function(data, averaging) {

  n <- sum(data)

  tp   <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn   <- n - (tpfp + tpfn - tp)
  fp   <- tpfp - tp

  numer <- tn
  denom <- tn + fp

  if(any(denom <= 0)) {
    res <- rep(NA_real_, times = nrow(data))
    return(res)
  }

  if(is_micro(averaging)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom
}


#' @rdname sens
#' @export
ppv <- function(data, ...) {
  UseMethod("ppv")
}

#' @rdname sens
#' @export
ppv.data.frame <- function(data, truth, estimate,
                           prevalence = NULL, averaging = "binary",
                           na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = construct_name("ppv", averaging),
    metric_fn = ppv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...,
    metric_fn_options = list(prevalence = prevalence)
  )

}

#' @rdname sens
#' @export
ppv.table <- function(data, prevalence = NULL, averaging = "binary",...) {

  ## "truth" in columns, predictions in rows
  check_table(data)
  data <- add_class(data, averaging)

  metric_tibbler(
    .metric = construct_name("ppv", averaging),
    .estimate = ppv_table_impl(data, prevalence = prevalence)
  )

}

#' @rdname sens
#' @export
ppv.matrix <- function(data, prevalence = NULL, averaging = "binary", ...) {

  data <- as.table(data)
  ppv.table(data, prevalence = prevalence, averaging = averaging)

}

#' @export
#' @rdname sens
ppv_vec <- function(truth, estimate, prevalence = NULL, averaging = "binary",
                    na.rm = TRUE, ...) {

  ppv_impl <- function(truth, estimate, prevalence) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )

    # # What is this doing?
    # lev <- if (getOption("yardstick.event_first"))
    #   colnames(xtab)[1]
    # else
    #   colnames(xtab)[2]

    xtab <- add_class(xtab, averaging)
    ppv_table_impl(xtab, prevalence = prevalence)

  }

  metric_vec_template(
    metric_impl = ppv_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...,
    prevalence = prevalence
  )

}

ppv_table_impl <- function(data, prevalence = NULL) {
  UseMethod("ppv_table_impl")
}

ppv_table_impl.binary <- function(data, prevalence = NULL) {

  positive <- pos_val(data)

  if (is.null(prevalence))
    prevalence <- sum(data[, positive]) / sum(data)

  sens <- sens_table_impl(data)
  spec <- spec_table_impl(data)
  (sens * prevalence) / ((sens * prevalence) + ((1 - spec) * (1 - prevalence)))

}

ppv_table_impl.macro <- function(data, prevalence = NULL) {

  .diag <- diag(data)
  .col_sums <- colSums(data)
  .row_sums <- rowSums(data)
  .n <- nrow(data)

  if(is.null(prevalence)) {
    prevalence <- .col_sums / sum(.col_sums)
  }

  .sens_vec <- .diag / .col_sums
  .spec_vec <- .diag / .row_sums

  numer <- .sens_vec * prevalence
  denom <- .sens_vec * prevalence + (1 - .spec_vec) * (1 - prevalence)

  sum(numer / denom) / .n
}



#' @rdname sens
#' @export
npv <- function(data, ...) {
  UseMethod("npv")
}

#' @rdname sens
#' @export
npv.data.frame <- function(data, truth, estimate,
                           prevalence = NULL, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "npv",
    metric_fn = npv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...,
    metric_fn_options = list(prevalence = prevalence)
  )

}

#' @rdname sens
#' @export
npv.table <- function(data, prevalence = NULL, ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = "npv",
    .estimate = npv_table_impl(data, prevalence = prevalence)
  )

}

#' @rdname sens
#' @export
npv.matrix <- function(data, prevalence = NULL, ...) {

  data <- as.table(data)
  npv.table(data, prevalence = prevalence)

}

#' @export
#' @rdname sens
npv_vec <- function(truth, estimate, prevalence = NULL, na.rm = TRUE, ...) {

  npv_impl <- function(truth, estimate, prevalence) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )

    # # What is this doing?
    # lev <- if (getOption("yardstick.event_first"))
    #   colnames(xtab)[1]
    # else
    #   colnames(xtab)[2]

    npv_table_impl(xtab, prevalence = prevalence)

  }

  metric_vec_template(
    metric_impl = npv_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...,
    prevalence = prevalence
  )

}

npv_table_impl <- function(data, prevalence = NULL) {

  positive <- pos_val(data)
  negative <- neg_val(data)

  if (is.null(prevalence))
    prevalence <- sum(data[, positive]) / sum(data)

  sens <- sens_table_impl(data)
  spec <- spec_table_impl(data)
  (spec * (1 - prevalence)) / (((1 - sens) * prevalence) + ((spec) * (1 - prevalence)))

}
