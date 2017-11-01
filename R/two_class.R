#' Other Metrics for 2x2 Tables
#' 
#' General metrics for two class problems that are not already
#'  in [sens()] or [recall()] are here, such as the Matthews
#'  correlation coefficient, Youden's J
#' 
#' @inheritParams sens
#' @author Max Kuhn
#' @seealso [conf_mat()]

#' @export 
mcc <- function(data, ...)
  UseMethod("mcc")

#' @export
#' @rdname mcc
mcc.data.frame  <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    mcc.table(xtab, ...)
  }

#' @rdname mcc
#' @export
"mcc.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    
    positive <- pos_val(data)
    negative <- neg_val(data)
    
    tp <- data[positive, positive]
    tn <- data[negative, negative]
    fp <- data[positive, negative]
    fn <- data[negative, positive]
    d1 <- tp + fp
    d2 <- tp + fn
    d3 <- tn + fp
    d4 <- tn + fn
    if (d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0)
      return(NA)
    ((tp * tn) - (fp * fn)) / sqrt(d1 * d2 * d3 * d4)
  }

#' @export 
j_index <- function(data, ...)
  UseMethod("j_index")

#' @export
#' @rdname mcc
j_index.data.frame  <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
      na.rm = na.rm,
      two_class = TRUE,
      dnn = c("Prediction", "Truth"),
      ...
    )
    j_index.table(xtab, ...)
  }

#' @rdname mcc
#' @export
"j_index.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)
    sens(data) + spec(data) - 1
  }

