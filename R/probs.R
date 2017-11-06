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

#'
#' @aliases roc_auc roc_auc.default pr_auc pr_auc.default 
#' @param data A data frame with the relevant columns.  
#' @param truth A single character value containing the column
#'  name of `data` that contains the true classes (in a factor).
#' @param estimate A character vector containing the columns
#'  name of `data` that contain the predicted class probabilities
#'  or some other score (in numeric vectors). These values are
#'  assumed to have larger values associated with the event,
#'  although this can be changed by passing the `direction` argument
#'  to [roc()] via the `...` when computing the ROC curve. If
#'  left `NULL`, the levels of the `truth` column are used. 
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds
#' @param ... Options to pass to [roc()] such as `direction` or 
#'  `smooth`. These options should not include `response`, 
#'  `predictor`, or `levels`. No options are available to pass to
#'  [PRAUC()].
#' @return A number between 0 and 1 (or NA) for `roc_auc` or
#'  `pr_auc`. For `mnLogLoss` a number of `NA`.
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [mcc()]
#' @keywords manip
#' @examples 
#' data("two_class_example")
#' prob_cols <- levels(two_class_example$truth)
#' 
#' roc_auc(two_class_example, truth = "truth", estimate = prob_cols)
#' roc_auc(two_class_example, truth = "truth", estimate = prob_cols,
#'         smooth = TRUE)
#'         
#' pr_auc(two_class_example, truth = "truth", estimate = prob_cols)    
#' 
#' mnLogLoss(two_class_example, truth = "truth", estimate = prob_cols)            

#' @export roc_auc
roc_auc <- function(data, ...)
  UseMethod("roc_auc")

#' @export
#' @rdname roc_auc
#' @importFrom pROC roc auc
roc_auc.data.frame  <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_factor(data[[truth]])
    
    lvl_values <- levels(data[[truth]])
    if(is.null(estimate))
      estimate <- lvl_values
    
    check_probs(data, estimate)
    
    if (getOption("yardstick.event_first")) {
      lvl <- rev(lvl_values)
      col <- lvl_values[1] 
    } else {
      lvl <- lvl_values
      col <- lvl_values[2] 
    }
    
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    
    curv <- pROC::roc(
      response = data[[truth]],
      predictor = data[, col ],
      levels = lvl, 
      ...
    )
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
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_factor(data[[truth]])
    
    lvl_values <- levels(data[[truth]])
    if(is.null(estimate))
      estimate <- lvl_values
    
    check_probs(data, estimate)
    
    data <- data[, c(truth, estimate)]
    
    if (na.rm)
      data <- data[complete.cases(data), ]
    
    pos <- if (getOption("yardstick.event_first"))
      lvl_values[1]
    else
      lvl_values[2]
    
    data[[truth]] <- ifelse(data[[truth]] == pos, 1, 0)
    
    res <- MLmetrics::PRAUC(
      y_true = data[[truth]],
      y_pred = data[[pos]]
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
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, sum = FALSE, ...) {
    check_factor(data[[truth]])
    
    lvl_values <- levels(data[[truth]])
    if(is.null(estimate))
      estimate <- lvl_values
    
    check_probs(data, estimate)
    
    lvl <- if (getOption("yardstick.event_first"))
      rev(lvl_values)
    else
      lvl_values
    
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data),]
    
    y <- model.matrix(~ data[[truth]] - 1)
    res <- y * as.matrix(data[, estimate])
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