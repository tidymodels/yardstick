#' Classification Metrics on Predited Classes
#' 
#' General metrics for classification models
#' 
#' @inheritParams sens
#' @author Max Kuhn
#' @seealso [conf_mat()]

#' @keywords manip
#' @export 
accuracy <- function(data, ...)
  UseMethod("accuracy")

#' @export
#' @rdname accuracy
accuracy.data.frame  <-
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
      dnn = c("Prediction", "Truth"),
      ...
    )
    accuracy.table(xtab, ...)
  }

#' @rdname accuracy
#' @export
"accuracy.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    sum(diag(data))/sum(data)
  }

#' @rdname accuracy
"accuracy.matrix" <-
  function(data, ...) {
    data <- as.table(data)
    accuracy.table(data)
  }

