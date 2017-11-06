#' Calculate sensitivity, specificity and predictive values
#'
#' These functions calculate the sensitivity, specificity or
#'  predictive values of a measurement system compared to a
#'  reference results (the truth or a gold standard). The
#'  measurement and "truth" data must have the same two possible
#'  outcomes and one of the outcomes must be thought of as a
#'  "positive" results or the "event".
#'
#' The sensitivity is defined as the proportion of positive
#'  results out of the number of samples which were actually
#'  positive. When there are no positive results, sensitivity is not
#'  defined and a value of `NA` is returned. Similarly, when
#'  there are no negative results, specificity is not defined and a
#'  value of `NA` is returned. Similar statements are true for
#'  predictive values. 
#'
#' The positive predictive value is defined as the percent of
#'  predicted positives that are actually positive while the
#'  negative predictive value is defined as the percent of negative
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
#' Suppose a 2x2 table with notation
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
#' @param data For the default functions, a factor containing the
#'  discrete measurements. For the `table` or `matrix`
#'  functions, a table or matrix object, respectively, where the
#'  true class results should be in the columns of the table. 
#' @param truth The column identifier for the true class results
#'  (that is a factor). This should an unquoted column name although
#'  this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names or column positions).
#' @param estimate The column identifier for the predicted class
#'  results (that is also factor). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#' @param prevalence A numeric value for the rate of the
#'  "positive" class of the data.
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds
#' @param ... Not currently used.
#' @return A number between 0 and 1 (or NA).
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [mcc()]
#' @references Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#'  sensitivity and specificity,'' *British Medical Journal*,
#'  vol 308, 1552.
#'
#'   Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2:
#'  predictive values,'' *British Medical Journal*, vol 309,
#'  102.
#' @keywords manip
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
#' @export sens
sens <- function(data, ...)
  UseMethod("sens")

#' @export
#' @rdname sens
sens.data.frame  <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      factor_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
  
    xtab <- vec2table(
      truth = data[[vars$truth]],
      estimate = data[[vars$estimate]],
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    sens.table(xtab, ...)
  }

#' @rdname sens
#' @export
"sens.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    positive <- pos_val(data)
    numer <- sum(data[positive, positive])
    denom <- sum(data[, positive])
    sens <- ifelse(denom > 0, numer / denom, NA)
    sens
  }

#' @rdname sens
"sens.matrix" <-
  function(data, ...) {
    data <- as.table(data)
    sens.table(data)
  }


#' @export
spec <-  function(data, ...)
  UseMethod("spec")


#' @export
#' @rdname sens
spec.data.frame  <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      factor_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    
    xtab <- vec2table(
      truth = data[[vars$truth]],
      estimate = data[[vars$estimate]],
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    
    spec.table(xtab, ...)
  }


#' @export
"spec.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    negative <- neg_val(data)
    
    numer <- sum(data[negative, negative])
    denom <- sum(data[, negative])
    spec <- ifelse(denom > 0, numer / denom, NA)
    spec
  }

"spec.matrix" <-
  function(data, ...) {
    data <- as.table(data)
    spec.table(data)
  }

#' @rdname sens
#' @export
ppv <- function(data, ...)
  UseMethod("ppv")

#' @export
ppv.data.frame  <-
  function(data, truth, estimate, 
           na.rm = TRUE, prevalence = NULL, ...) {
    vars <-
      factor_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    
    xtab <- vec2table(
      truth = data[[vars$truth]],
      estimate = data[[vars$estimate]],
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    lev <- if (getOption("yardstick.event_first"))
      colnames(xtab)[1]
    else
      colnames(xtab)[2]
    
    ppv.table(xtab, prevalence = prevalence, ...)
  }

#' @rdname sens
#' @export
"ppv.table" <-
  function(data, prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    positive <- pos_val(data)
    negative <- neg_val(data)
    
    if (is.null(prevalence))
      prevalence <- sum(data[, positive]) / sum(data)
    
    sens <- sens(data)
    spec <- spec(data)
    (sens * prevalence) / ((sens * prevalence) + ((1 - spec) * (1 - prevalence)))
    
  }

#' @rdname sens
#' @export
"ppv.matrix" <-
  function(data, prevalence = NULL, ...) {
    data <- as.table(data)
    ppv.table(data, prevalence = prevalence)
  }

#' @rdname sens
#' @export
npv <- function(data, ...)
  UseMethod("npv")

#' @export
npv.data.frame  <-
  function(data, truth, estimate, 
           na.rm = TRUE, prevalence = NULL, ...) {
    vars <-
      factor_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    
    xtab <- vec2table(
      truth = data[[vars$truth]],
      estimate = data[[vars$estimate]],
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    lev <- if (getOption("yardstick.event_first"))
      colnames(xtab)[2]
    else
      colnames(xtab)[1]
    
    npv.table(xtab, prevalence = prevalence, ...)
  }

#' @rdname sens
#' @export
"npv.table" <-
  function(data, prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    positive <- pos_val(data)
    negative <- neg_val(data)
    
    if (is.null(prevalence))
      prevalence <- sum(data[, positive]) / sum(data)
    
    sens <- sens(data)
    spec <- spec(data)
    (spec * (1 - prevalence)) / (((1 - sens) * prevalence) + ((spec) * (1 - prevalence)))
    
  }

#' @rdname sens
#' @export
"npv.matrix" <-
  function(data, prevalence = NULL, ...) {
    data <- as.table(data)
    npv.table(data, prevalence = prevalence)
  }

