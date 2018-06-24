#' Metrics Based on Class Probabilities
#'
#' These functions compute the areas under the receiver operating
#'  characteristic (ROC) curve (`roc_auc()`), the precision-recall
#'  curve (`pr_auc()`), or the multinomial log loss (`mnLogLoss()`). The actual ROC
#'  curve can be created using `roc_curve()`. The actual PR curve can be created
#'  using `pr_curve()`.
#'
#' There is no common convention on which factor level should
#'  automatically be considered the "relevant" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.

#' @inheritParams sens
#'
#' @aliases roc_auc roc_auc.default pr_auc pr_auc.default roc_curve pr_curve
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#' @param estimate The column identifier for the predicted class probabilities
#' (that is a `numeric`) corresponding to the "positive" result. See Details.
#' For `_vec()` functions, a `numeric` vector. For `mnLogLoss_vec`, this should
#' be a matrix with as many columns as factor levels in `truth`.
#' @param ... For `mnLogLoss()`, a set of unquoted column names or one or more
#'  `dplyr` selector functions to choose which variables contain the
#'  class probabilities. There should be as many columns as
#'  factor levels of `truth`. It is **assumed** that they are in the
#'  same order as the factor levels. Otherwise, unused.
#' @param options A `list` of named options to pass to [roc()]
#' such as `direction` or `smooth`. These options should not include `response`,
#' `predictor`, or `levels`.
#'
#' @return
#'
#' For `_vec()` functions, a single `numeric` value (or `NA`). Otherwise, a
#' `tibble` with columns `.metric` and `.estimate` and 1 row of
#' values.
#'
#' For grouped data frames, the number of rows returned will be the
#' same as the number of groups.
#'
#' For `roc_curve()`, a tibble with columns
#' `sensitivity` and `specificity`. If an ordinary (i.e. non-smoothed) curve
#' is used, there is also a column for `threshold`.
#'
#' For `pr_curve()`, a tibble with columns `recall`, `precision`, and
#' `threshold`.
#'
#' @details
#'
#' `roc_curve()` computes the sensitivity at every unique
#'  value of the probability column (in addition to infinity and
#'  minus infinity). If a smooth ROC curve was produced, the unique
#'  observed values of the specificity are used to create the curve
#'  points. In either case, this may not be efficient for large data
#'  sets.
#'
#'  `pr_curve()` computes the precision at every unique value of the
#'  probability column (in addition to infinity).
#'
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [mcc()]
#' @keywords manip
#' @examples
#' library(tidyselect)
#'
#' data("two_class_example")
#' prob_cols <- levels(two_class_example$truth)
#'
#' roc_auc(two_class_example, truth = truth, Class1)
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' roc_curve(two_class_example, truth, Class1) %>%
#'   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#'   geom_path() +
#'   geom_abline(lty = 3) +
#'   coord_equal() +
#'   theme_bw()
#'
#' pr_curve(two_class_example, truth, Class1) %>%
#'   ggplot(aes(x = recall, y = precision)) +
#'   geom_path() +
#'   coord_equal() +
#'   theme_bw()
#'
#' # passing options via a list and _not_ `...`
#' roc_auc(two_class_example, truth = truth, Class1,
#'         options = list(smooth = TRUE))
#'
#'
#' pr_auc(two_class_example, truth, Class1)
#'
#' mnLogLoss(two_class_example, truth, starts_with("Class"))
#' # or
#' mnLogLoss(two_class_example, truth, !! prob_cols)

#' @export roc_auc
roc_auc <- function(data, ...)
  UseMethod("roc_auc")

#' @export
#' @rdname roc_auc
roc_auc.data.frame  <- function(data, truth, estimate, options = list(), na.rm = TRUE, ...) {

    metric_summarizer(
      metric_nm = "roc_auc",
      metric_fn = roc_auc_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na.rm = na.rm,
      ... = ...,
      metric_fn_options = list(options = options)
    )

}

#' @rdname roc_auc
#' @export
#' @importFrom rlang call2
#' @importFrom pROC roc auc
roc_auc_vec <- function(truth, estimate, options = list(), na.rm = TRUE, ...) {

  roc_auc_impl <- function(truth, estimate) {

    lvl_values <- levels(truth)

    if (getOption("yardstick.event_first")) {
      lvl <- rev(lvl_values)
    } else {
      lvl <- lvl_values
    }

    args <- quos(response = truth, predictor = estimate, levels = lvl)

    curv <- eval_tidy(call2("roc", !!! args, !!! options, .ns = "pROC"))

    res <- unname(pROC::auc(curv))

    as.numeric(res)
  }

  metric_vec_template(
    metric_impl = roc_auc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = c("factor", "numeric"),
    ...
  )
}

#' @export
#' @rdname roc_auc
pr_auc <- function(data, ...)
  UseMethod("pr_auc")

#' @export
#' @rdname roc_auc
pr_auc.data.frame  <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "pr_auc",
    metric_fn = pr_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname roc_auc
pr_auc_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  pr_auc_impl <- function(truth, estimate) {

    pr_list <- pr_curve_vec(truth, estimate, na.rm)
    auc(pr_list[["recall"]], pr_list[["precision"]])

  }

  metric_vec_template(
    metric_impl = pr_auc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = c("factor", "numeric"),
    ...
  )

}

#' @export mnLogLoss
#' @rdname roc_auc
mnLogLoss <- function(data, ...)
  UseMethod("mnLogLoss")

#' @export
#' @rdname roc_auc
#' @param sum A `logical`. Should the sum of the likelihood contributions be
#' returned (instead of the mean value)?
#' @importFrom rlang quo
mnLogLoss.data.frame <- function(data, truth, ..., na.rm = TRUE, sum = FALSE) {

    # Capture dots
    dot_vars <- rlang::with_handlers(
      tidyselect::vars_select(names(data), !!! quos(...)),
      tidyselect_empty = abort_selection
    )

    # estimate is a matrix of the selected columns
    dot_nms <- lapply(dot_vars, as.name)
    estimate <- quo(matrix(c(!!! dot_nms), ncol = !!length(dot_nms)))

    metric_summarizer(
      metric_nm = "mnLogLoss",
      metric_fn = mnLogLoss_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!estimate,
      na.rm = na.rm,
      # dots are captured for column names in this impl
      #... = ...,
      # Extra argument for mnLogLoss_impl()
      metric_fn_options = list(sum = sum)
    )

  }

#' @rdname roc_auc
#' @importFrom stats model.matrix
#' @export
mnLogLoss_vec <- function(truth, estimate, na.rm = TRUE, sum = FALSE, ...) {

  # estimate here is a matrix of class prob columns
  mnLogLoss_impl <- function(truth, estimate, na.rm = TRUE, sum = FALSE) {

    lvl <- levels(truth)

    if (NCOL(estimate) != length(lvl)) {
      stop(
        "`estimate` should contain ",
        length(lvl),
        " columns of probabilities",
        call. = FALSE
      )
    }

    y <- model.matrix(~ truth - 1)
    res <- y * estimate
    res[res <= .Machine$double.eps & res > 0] <- .Machine$double.eps
    pos_log <- function(x)
      -log(x[x != 0])
    res <- sum(apply(res, 1, pos_log))
    if (!sum)
      res <- res / length(truth)
    res

  }

  metric_vec_template(
    metric_impl = mnLogLoss_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = c("factor", "matrix"),
    ...,
    sum = sum
  )
}

#' @export
#' @rdname roc_auc
roc_curve <- function(data, ...)
  UseMethod("roc_curve")

#' @export
#' @rdname roc_auc
#' @importFrom pROC coords
#' @importFrom rlang invoke
#' @importFrom dplyr arrange as_tibble %>%
roc_curve.data.frame  <- function (data, truth, estimate, options = list(), na.rm = TRUE, ...) {
  vars <-
    prob_select(
      data = data,
      truth = !!enquo(truth),
      !!enquo(estimate) # currently passed as dots
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
  if (!inherits(curv, "smooth.roc")) {
    res <- coords(
      curv,
      x = unique(c(-Inf, options$predictor, Inf)),
      input = "threshold"
    )
  } else {
    res <- coords(
      curv,
      x = unique(c(0, curv$specificities, 1)),
      input = "specificity"
    )
  }
  res <- dplyr::as_tibble(t(res))
  res <- if (!inherits(curv, "smooth.roc"))
    res %>% dplyr::arrange(threshold)
  else
    res %>% dplyr::arrange(specificity)
  res
}

#' @export
#' @rdname roc_auc
pr_curve <- function(data, ...) {
  UseMethod("pr_curve")
}

#' @export
#' @rdname roc_auc
#' @importFrom stats relevel
pr_curve.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  vars <- prob_select(
    data = data,
    truth = !!enquo(truth),
    !!enquo(estimate) # currently passed as dots
  )

  truth <- data[[vars$truth]]
  estimate <- data[[vars$probs]]

  pr_list <- pr_curve_vec(truth, estimate, na.rm)

  tibble::tibble(!!!pr_list)
}

# Undecided of whether to export this or not
pr_curve_vec <- function(truth, estimate, na.rm) {

  lvls <- levels(truth)

  if(length(lvls) != 2L) {
    stop("`truth` must be a two level factor.", call. = FALSE)
  }

  # Relevel if event_first = FALSE
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!getOption("yardstick.event_first")) {
    truth <- relevel(truth, lvls[2])
  }

  if(na.rm) {
    complete_idx <- complete.cases(truth, estimate)
    truth <- truth[complete_idx]
    estimate <- estimate[complete_idx]
  }

  # quicker to convert to integer now rather than letting rcpp do it
  # 1=good, 2=bad
  truth <- as.integer(truth)

  pr_list <- pr_curve_cpp(truth, estimate)

  pr_list
}

# AUC by trapezoidal rule:
# https://en.wikipedia.org/wiki/Trapezoidal_rule
# assumes x is a partition and that x & y are the same length
auc <- function(x, y, na.rm = TRUE) {

  if(na.rm) {
    comp <- complete.cases(x, y)
    x <- x[comp]
    y <- y[comp]
  }

  # order increasing by x
  x_order <- order(x)
  x <- x[x_order]
  y <- y[x_order]

  # length x = length y
  n <- length(x)

  # dx
  dx <- x[-1] - x[-n]

  # mid height of y
  height <- (y[-n] + y[-1]) / 2

  auc <- sum(height * dx)

  auc
}

#' @importFrom utils globalVariables
utils::globalVariables(c("estimate", "threshold", "specificity"))
