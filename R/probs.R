#' Metrics Based on Class Probabilities
#'
#' These functions compute the areas under the receiver operating
#'  characteristic (ROC) curve (`roc_auc`), the precision-recall
#'  curve (`pr_auc`), or the multinomial log loss (`mnLogLoss`).
#' 
#' There is no common convention on which factor level should
#'  automatically be considered the "relevant" or "positive" results. 
#'  In `yardstick`, the default is to use the _first_ level. To 
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest. 

#' @inheritParams sens
#' @aliases roc_auc roc_auc.default pr_auc pr_auc.default 
#' @param data A data frame with the relevant columns.  
#' @param ... A set of unquoted column names or one or more
#'  `dplyr` selector functions to choose which variables contain the
#'  class probabilities. See the examples below. For `roc_auc` and
#'  `pr_auc`, only one value is required. If more are given, the 
#'  functions will try to match the column name to the appropriate
#'  factor level of `truth`. If this doesn't work, an error is
#'  thrown. For `mnLogLoss`, there should be as many columns as
#'  factor levels of `truth`. It is **assumed** that they are in the
#'  same order as the factor levels. 
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds
#' @param options Options to pass to [roc()] such as `direction` or 
#'  `smooth`. These options should not include `response`, 
#'  `predictor`, or `levels`. 
#' @return A number between 0 and 1 (or NA) for `roc_auc` or
#'  `pr_auc`. For `mnLogLoss` a number or `NA`.
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [mcc()]
#' @keywords manip
#' @examples 
#' library(tidyselect)
#' 
#' data("two_class_example")
#' prob_cols <- levels(two_class_example$truth)
#' 
#' roc_auc(two_class_example, truth = truth, Class1)
#' # warning is issued here because 2 columns are selected:
#' roc_auc(two_class_example, truth, starts_with("Class"))
#' 
#' # passing options via a list and _not_ `...`
#' roc_auc(two_class_example, truth = "truth", Class1,
#'         options = list(smooth = TRUE))
#'         
#' pr_auc(two_class_example, truth, prob_cols)    
#' 
#' mnLogLoss(two_class_example, truth, starts_with("Class"))
#' # or
#' mnLogLoss(two_class_example, truth, !! prob_cols)            

#' @export roc_auc
roc_auc <- function(data, ...)
  UseMethod("roc_auc")

#' @export
#' @rdname roc_auc
#' @importFrom pROC roc auc
#' @importFrom rlang invoke
roc_auc.data.frame  <-
  function(data, truth, ..., options = list(), na.rm = TRUE) {
    vars <-
      prob_select(
        data = data,
        truth = !!enquo(truth),
        ...
      )
    
    lvl_values <- levels(data[[vars$truth]])

    if (getOption("yardstick.event_first")) {
      lvl <- rev(lvl_values)
    } else {
      lvl <- lvl_values
    }
    col <- match_levels_to_cols(vars$probs, rev(lvl))
    
    data <- data[, c(vars$truth, col)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    
    # working on a better way of doing this
    options$response <- data[[vars$truth]]
    options$predictor <- data[[col]]
    options$levels <- lvl
    
    curv <- invoke(pROC::roc, options)
    res <- unname(pROC::auc(curv))
    as.numeric(res)
  }

#' @export 
#' @rdname roc_auc
pr_auc <- function(data, ...)
  UseMethod("pr_auc")

#' @export
#' @rdname roc_auc
#' @importFrom MLmetrics PRAUC
pr_auc.data.frame  <-
  function(data, truth, ..., na.rm = TRUE) {
    vars <-
      prob_select(
        data = data,
        truth = !!enquo(truth),
        ...
      )
    
    lvl_values <- levels(data[[vars$truth]])
    
    if (getOption("yardstick.event_first")) {
      lvl <- lvl_values
    } else {
      lvl <- rev(lvl_values)
    }
    col <- match_levels_to_cols(vars$probs, lvl)
    
    data <- data[, c(vars$truth, col)]
    if (na.rm)
      data <- data[complete.cases(data), ]

    data[[vars$truth]] <- ifelse(data[[vars$truth]] == lvl[1], 1, 0)
    
    res <- MLmetrics::PRAUC(
      y_true = data[[vars$truth]],
      y_pred = data[[col]]
    )
    res
  }

#' @export mnLogLoss
#' @rdname roc_auc
mnLogLoss <- function(data, ...)
  UseMethod("mnLogLoss")

#' @export
#' @rdname roc_auc
#' @importFrom stats model.matrix
#' @param sum A logical. Should the sum of the likelihood
#'  contrinbutions be returned (instead of the mean value)?
mnLogLoss.data.frame  <-
  function(data, truth, ..., na.rm = TRUE, sum = FALSE) {
    vars <-
      prob_select(
        data = data,
        truth = !!enquo(truth),
        ...
      )
    
    lvl <- levels(data[[vars$truth]])
    
    if (length(vars$probs) != length(lvl))
      stop("`...` should select exactly ",
           length(lvl),
           " columns of probabilities",
           call. = FALSE)
    
    data <- data[, c(vars$truth, vars$probs)]
    if (na.rm)
      data <- data[complete.cases(data), ]

    y <- model.matrix(~ data[[vars$truth]] - 1)
    res <- y * as.matrix(data[, vars$probs])
    res[res <= .Machine$double.eps & res > 0] <- .Machine$double.eps
    pos_log <- function(x)
      log(x[x != 0])
    res <- sum(apply(res, 1, pos_log))
    if (!sum)
      res <- res / nrow(data)
    res
  }

#' @importFrom utils globalVariables
utils::globalVariables(c("estimate"))