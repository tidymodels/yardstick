#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance
#'  estimates depending on the class of `truth` (see **Value**
#'  below) and returns them in a single row tibble.
#'
#' @param data A data frame
#' @param truth The column identifier for the true results (that
#'  is numeric or factor). This should an unquoted column name
#'  although this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names or column positions).
#' @param estimate The column identifier for the predicted results
#'  (that is also numeric or factor). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#' @param ... For classification: a set of unquoted column names
#'  or one or more `dplyr` selector functions to choose which
#'  variables contain the class probabilities. See the examples
#'  below. For `roc_auc` and `pr_auc`, only one value is required.
#'  If more are given, the functions will try to match the column
#'  name to the appropriate factor level of `truth`. If this doesn't
#'  work, an error is thrown. For `mnLogLoss`, there should be as
#'  many columns as factor levels of `truth`. It is **assumed** that
#'  they are in the same order as the factor levels.
#' @param options Options to pass to [roc()] such as `direction` or
#'  `smooth`. These options should not include `response`,
#'  `predictor`, or `levels`.
#' @return A single row tibble. When `truth` is a factor, there is
#'  an [accuracy()] column. If a full set of class probability
#'  columns are passed to `...`, then there is also a column for
#'  [mnLogLoss()]. When `truth` has two levels and there are class
#'  probabilities, [roc_auc()] is appended. When `truth` is numeric,
#'  there are columns for [rmse()] and [rsq()],
#' @return A number or `NA`

#' @export metrics
metrics <- function(data, ...)
  UseMethod("metrics")

#' @export
#' @rdname metrics
#' @importFrom dplyr tibble
metrics.data.frame  <-
  function(data, truth, estimate, ..., options = list(), na.rm = TRUE) {
    vars <-
      all_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    all_vars <- unique(unlist(vars))
    all_vars <- all_vars[!is.na(all_vars)]

    data <- data[, all_vars]
    if (na.rm)
      data <- data[complete.cases(data), ]

    is_class <- is.factor(data[[ vars$truth ]])
    if (is_class) {
      if(!is.factor(data[[ vars$estimate ]]))
        stop("`estimate` should be a factor", call. = FALSE)

      xtab <- vec2table(
        truth = data[[ vars$truth ]],
        estimate = data[[ vars$estimate ]],
        na.rm = na.rm,
        dnn = c("Truth", "Prediction")
      )

      res <- dplyr::tibble(accuracy = accuracy(xtab))

      has_probs <- !all(is.na(vars$probs))
      if (has_probs) {
        res$mnLogLoss <-
          mnLogLoss(data, vars$truth, !! vars$probs, na.rm = na.rm)

        lvl <- levels(data[[ vars$truth ]])
        if (length(lvl) == 2) {
          col <- if (getOption("yardstick.event_first"))
            lvl[1]
          else
            lvl[2]
          res$roc_auc <-
            roc_auc(
              data, vars$truth,
              !! col,
              na.rm = na.rm,
              options = options
            )

        } # end two_classes

      } # end has_probs

    } else {
      # Assume only regression for now
      if(!is.numeric(data[[ vars$estimate ]]))
        stop("`estimate` should be numeric", call. = FALSE)

      res <- dplyr::tibble(
        rmse = rmse_calc(data[[ vars$truth ]], data[[ vars$estimate ]]),
        rsq = rsq_calc(data[[ vars$truth ]], data[[ vars$estimate ]])
      )

    } # end regression

    res
  }

