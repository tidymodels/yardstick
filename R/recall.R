#' Calculate recall, precision and F values
#'
#' These functions calculate the recall, precision or F values of
#'  a measurement system for finding/retrieving relevant documents
#'  compared to reference results (the truth regarding relevance).
#'  The measurement and "truth" data must have the same two possible
#'  outcomes and one of the outcomes must be thought of as a
#'  "relevant" results.
#'
#' The recall (aka specificity) is defined as the proportion of
#'  relevant results out of the number of samples which were
#'  actually relevant. When there are no relevant results, recall is
#'  not defined and a value of `NA` is returned.
#'
#' The precision is percentage of predicted truly relevant results
#'  of the total number of predicted relevant results and
#'  characterizes the "purity in retrieval performance" (Buckland
#'  and Gey, 1994).
#'
#' The measure "F" is a combination of precision and recall (see
#'  below).
#'
#' Suppose a 2x2 table with notation
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab relevant \tab
#' Irrelevant \cr relevant \tab A \tab B \cr Irrelevant \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{recall = A/(A+C)} \deqn{precision =
#' A/(A+B)} \deqn{F_i = (1+i^2)*prec*recall/((i^2 * precision)+recall)}
#'
#' See the references for discussions of the statistics.
#'
#' @aliases recall recall.default recall.table precision
#'  precision.default precision.table precision.matrix F_meas
#'  F_meas.default F_meas.table
#' @param data For the default functions, a factor containing the
#'  discrete measurements. For the `table` function, a table.
#' @param truth A factor containing the reference values (i.e.
#'  truth).
#' @param beta A numeric value used to weight precision and
#'  recall. A value of 1 is traditionally used and corresponds to
#'  the harmonic mean of the two values but other values weight
#'  recall beta times more important than precision.
#' @param na.rm A logical value indicating whether `NA` values
#'  should be stripped before the computation proceeds.
#' @param ... Not currently used.
#' @return A number between 0 and 1 (or NA).
#' @author Max Kuhn
#' @seealso [conf_mat()]
#' @references Buckland, M., & Gey, F. (1994). The relationship
#'  between Recall and Precision. *Journal of the American Society
#'  for Information Science*, 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F
#'  Factor to ROC, Informedness, Markedness and Correlation.
#'  Technical Report SIE-07-001, Flinders University
#' @keywords manip
#' @examples
#'
#'
#' @export recall
recall <- function(data, ...)
  UseMethod("recall")

#' @rdname recall
#' @export
"recall.table" <-
  function(data, ...) {
    check_table(data)
    
    relevant <- pos_val(data)
    
    numer <- data[relevant, relevant]
    denom <- sum(data[, relevant])
    rec <- ifelse(denom > 0, numer / denom, NA)
    rec
  }

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
recall.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    yardstick::recall.table(xtab)
  }

#' @rdname recall
#' @export
precision <- function(data, ...)
  UseMethod("precision")

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
precision.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    yardstick::precision.table(xtab)
  }

#' @rdname recall
#' @export
precision.table <- function (data, ...) {
  check_table(data)
  
  relevant <- pos_val(data)
  numer <- data[relevant, relevant]
  denom <- sum(data[relevant, ])
  precision <- ifelse(denom > 0, numer / denom, NA)
  precision
}

#' @rdname recall
#' @export
F_meas <- function(data, ...)
  UseMethod("F_meas")

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
F_meas.default <-
  function(data, truth = NULL, estimate = NULL, beta = 1, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    yardstick::F_meas.table(xtab, beta = beta)
  }

#' @rdname recall
#' @export
F_meas.table <-
  function (data, beta = 1, ...) {
    check_table(data)
    
    relevant <- pos_val(data)
    precision <- precision.table(data, relevant = relevant)
    rec <- recall.table(data, relevant = relevant)
    (1 + beta ^ 2) * precision * rec / ((beta ^ 2 * precision) + rec)
  }
