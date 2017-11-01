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
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
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

