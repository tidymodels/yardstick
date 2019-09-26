#' Calculate concordance, censoring-adjusted concordance and censoring-adjusted AUC for survival models
#'
#' These functions calculate the
#'
#' The concordance is defined as the
#'
#' The censoring-adjusted concordance is defined as the
#'
#' The survival AUC predictive value is defined as the
#'
#' See the references for discussions of the statistics.
#'
#'
#' @aliases concordance
#' @param data For the default functions, a factor containing the
#'  discrete measurements. For the `table` or `matrix`
#'  functions, a table or matrix object, respectively, where the
#'  true class results should be in the columns of the table.
#' @param truth The column identifier for the true class results
#'  (that is a Surv object). This should an unquoted column name although
#'  this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names or column positions).
#' @param training Surv object corresponding to the data used to train the
#'  model use to generate the predictions. Used to adjust the concordance
#'  statistic for the amount of censoring in the training data.
#' @param estimate The column identifier for the predicted hazard or predicted
#'  survival time (that is numeric). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#' @param invert A logical value, which controls whether or not the ordering of
#'  the "estimate" column is inverted before concordance or AUC is calculated.
#'  Importantly, this should be set to TRUE for predictions of hazard,
#'  and FALSE for predictions of survival time.
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds
#' @param ... Not currently used.
#' @return A number between 0 and 1 (or NA).
#' @seealso survival::survConcordance survAUC::UnoC survAUC::AUC.uno
#' @references
#'
#' Harrell, F. E., R. M. Califf, D. B. Pryor, K. L. Lee and R. A.
#' Rosati (1982).
#' Evaluating the yield of medical tests.
#' _Journal of the American Medical Association_ *247*, 2543-2546.
#'
#' Harrell, F. E., K. L. Lee, R. M. Califf, D. B. Pryor and R. A.
#' Rosati (1984).
#' Regression modeling strategies for improved prognostic prediction.
#' _Statistics in Medicine_ *3*, 143-152.
#'
#' Uno, H., T. Cai T, M. J. Pencina, R. B. D'Agostino and W. L. Wei
#' (2011).
#' On the C-statistics for evaluating overall adequacy of risk
#' prediction procedures with censored survival data.
#' _Statistics in Medicine_ *30*, 1105-1117.
#'
#' Uno H, Cai T, Pencina MJ, D’Agostino RB, Wei LJ.
#' On the C-statistics for Evaluating Overall Adequacy of Risk Prediction
#' Procedures with Censored Survival Data. Statistics in medicine.
#' 2011;30(10):1105-1117. doi:10.1002/sim.4154.
#'
#' Uno, H., T. Cai, L. Tian, and L. J. Wei (2007).
#' Evaluating prediction rules for t-year survivors with censored
#' regression models.
#' _Journal of the American Statistical Association_ *102*, 527-537.
#'
#' @keywords manip
#' @examples
#' data("surv_example")
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
#' @export
concordance <- function(data, ...)
  UseMethod("concordance")

#' @export
#' @rdname concordance
concordance.data.frame <- function(data,
                                   truth,
                                   estimate,
                                   training = NULL,
                                   invert = FALSE,
                                   na.rm = TRUE,
                                   ...) {

  vars <- surv_select(
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ...
  )
  if (na.rm) {
    ind_na <- sapply(seq_len(nrow(data)),
      function(i) {
        any(is.na(as.vector(data[i, c(vars$truth, vars$estimate)])))
      },
      FUN.VALUE = logical(1)
    )
    data <- data[!ind_na, ]
  }
  concordance(data[[vars$truth]], 
              data[[vars$estimate]],
              training = training,
              invert = invert,
              ...)
}

#' @export
concordance.Surv <- function(x, y, invert = FALSE, training = NULL, ...) {
  if (invert) {
    y <- -y
  }
  if (!is.null(training)) {
    if (!inherits(training, "Surv")) {
      stop("training must be a Surv object")
    }
    survAUC::UnoC(training, x, y)
  } else {
    survival::survConcordance(x ~ y)[["concordance"]][["concordant"]]
  }
}

#' Implements Uno's AUC.
#' 
#' @export
surv_auc <- function(data, ...)
  UseMethod("auc")

#' @export
surv_auc.data.frame <- function(data,
                                truth,
                                estimate,
                                invert = FALSE,
                                training = NULL,
                                times,
                                na.rm = TRUE,
                                ...) {

  vars <- surv_select(
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ...
  )
  if (na.rm) {
    ind_na <- vapply(seq_len(nrow(data)),
      function(i) {
        any(is.na(as.vector(data[i, c(vars$truth, vars$estimate)])))
      },
      FUN.VALUE = logical(1)
    )
    data <- data[!ind_na, ]
  }
  surv_auc(data[[vars$truth]],
      data[[vars$estimate]],
      training = training,
      times = times,
      invert = invert,
      ...)
}

#' @export
surv_auc.Surv <- function(x, y, training, times, invert, ...) {
  if (invert) {
    y <- -y
  }
  if (!inherits(training, "Surv")) {
    stop("training must be a Surv object")
  }

  data.frame(times, auc = survAUC::AUC.uno(training, x, y, times)$iauc)
}
