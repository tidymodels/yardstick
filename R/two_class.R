#' Other Metrics for 2x2 Tables
#'
#' Other metrics for two class problems that are not already in
#'  [sens()] or [recall()] are here, such as the Matthews
#'  correlation coefficient, Youden's J, the balanced accuracy (the
#'  average between sensitivity and specificity), and the detection
#'  prevalence (the rate of _predicted_ events).
#'
#' There is no common convention on which factor level should
#'  automatically be considered the "event" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' If more than one statistic is required, it is more
#'  computationally efficient to create the confusion matrix using
#'  [conf_mat()] and applying the corresponding `summary` method
#'  ([summary.conf_mat()]) to get the values at once.
#'
#' @inheritParams sens
#' @author Max Kuhn
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [sens()], [spec()]
#' @examples
#' data("two_class_example")
#'
#' mcc(two_class_example, truth, predicted)
#'
#' j_index(two_class_example, truth, predicted)
#'
#' bal_accuracy(two_class_example, truth, predicted)
#'
#' detection_prevalence(two_class_example, truth, predicted)
#' @export
mcc <- function(data, ...)
  UseMethod("mcc")

#' @export
#' @rdname mcc
mcc.data.frame  <-
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

    # This and `prod` below to deal with integer overflow
    data <- as.matrix(data)

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
    ((tp * tn) - (fp * fn)) / sqrt(prod(d1, d2, d3, d4))
  }

#' @export
#' @rdname mcc
j_index <- function(data, ...)
  UseMethod("j_index")

#' @export
#' @rdname mcc
j_index.data.frame  <-
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

#' @export
#' @rdname mcc
bal_accuracy <- function(data, ...)
  UseMethod("bal_accuracy")

#' @export
#' @rdname mcc
bal_accuracy.data.frame  <-
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
    bal_accuracy.table(xtab, ...)
  }

#' @rdname mcc
#' @export
"bal_accuracy.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)

    ( sens(data) + spec(data) ) / 2
  }

#' @rdname mcc
"bal_accuracy.matrix" <-
  function(data, ...) {
    data <- as.table(data)
    bal_accuracy.table(data)
  }

#' @export
#' @rdname mcc
detection_prevalence <- function(data, ...)
  UseMethod("detection_prevalence")

#' @export
#' @rdname mcc
detection_prevalence.data.frame  <-
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
    detection_prevalence.table(xtab, ...)
  }

#' @rdname mcc
#' @export
"detection_prevalence.table" <-
  function(data, ...) {
    ## "truth" in columns, predictions in rows
    check_table(data)

    pos_level <- pos_val(data)
    sum(data[pos_level, ])/sum(data)
  }

#' @rdname mcc
"detection_prevalence.matrix" <-
  function(data, ...) {
    data <- as.table(data)
    detection_prevalence.table(data)
  }

