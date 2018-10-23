#' Metrics Based on Class Probabilities
#'
#' These functions compute the areas under the receiver operating
#'  characteristic (ROC) curve (`roc_auc()`), the precision-recall
#'  curve (`pr_auc()`), or the multinomial log loss (`mn_log_loss()`). The actual ROC
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
#'
#' @inheritParams sens
#'
#' @aliases roc_auc roc_auc.default pr_auc pr_auc.default roc_curve pr_curve
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#'
#' @param estimate If `truth` is binary, a numeric vector of class probabilities
#' corresponding to the "relevant" class. Otherwise, a matrix with as many
#' columns as factor levels of `truth`.
#'
#' @param ... A set of unquoted column names or one or more
#' `dplyr` selector functions to choose which variables contain the
#' class probabilities. If truth is binary, only 1 column should be selected.
#' Otherwise, there should be as many columns as factor levels of `truth`.
#'
#' @param options A `list` of named options to pass to [roc()]
#' such as `direction` or `smooth`. These options should not include `response`,
#' `predictor`, or `levels`.
#'
#' @param averaging For `roc_auc()` and `pr_auc()`, one of `"binary"`,
#' `"macro"`, or `"macro_weighted"` to specify the type of averaging to be done.
#' `"binary"` is only relevant for the two class case. The other two are
#' general methods for calculating multiclass metrics. The default will
#' automatically choose `"binary"` or `"macro"` based on `estimate`. `roc_auc()`
#' also accepts `"hand_till"` for the metric described in Hand, Till (2001).
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
#' is used, there is also a column for `.threshold`.
#'
#' For `pr_curve()`, a tibble with columns `recall`, `precision`, and
#' `.threshold`.
#'
#' For `roc_curve()` and `pr_curve()`, if a multiclass `truth` column is
#' provided, a one-vs-all approach will be taken to calculate multiple curves,
#' one per level. In this case, there will be an additional column, `.level`,
#' identifying the "one" column in the one-vs-all calculation.
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
#' @name roc_auc
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
#' mn_log_loss(two_class_example, truth, Class1)
#' # or
#' mn_log_loss(two_class_example, truth, !! prob_cols[1])
#'
#' # Multiclass one-vs-all approach with roc_curve()
#' # One curve per level
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   roc_curve(obs, VF:L) %>%
#'   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#'   geom_path() +
#'   geom_abline(lty = 3) +
#'   coord_equal() +
#'   theme_bw() +
#'   facet_wrap(~.level)
#'
NULL

# ROC AUC ----------------------------------------------------------------------

#' @export
#' @rdname roc_auc
roc_auc <- function(data, ...) {
  UseMethod("roc_auc")
}

class(roc_auc) <- c("prob_metric", "function")

#' @export
#' @rdname roc_auc
roc_auc.data.frame  <- function(data, truth, ..., options = list(),
                                averaging = NULL, na.rm = TRUE) {


  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "roc_auc",
    metric_fn = roc_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    averaging = averaging,
    na.rm = na.rm,
    ... = ...,
    metric_fn_options = list(options = options)
  )

}

#' @rdname roc_auc
#' @export
#' @importFrom rlang call2
#' @importFrom pROC roc auc
roc_auc_vec <- function(truth, estimate, options = list(),
                        averaging = NULL, na.rm = TRUE, ...) {

  averaging <- finalize_averaging(truth, averaging)

  roc_auc_impl <- function(truth, estimate) {
    roc_auc_averaging_impl(truth, estimate, options, averaging)
  }

  metric_vec_template(
    metric_impl = roc_auc_impl,
    truth = truth,
    estimate = estimate,
    averaging = averaging,
    na.rm = na.rm,
    cls = c("factor", "numeric"),
    averaging_override = c("binary", "macro", "macro_weighted", "hand_till"),
    ...
  )
}

roc_auc_averaging_impl <- function(truth, estimate, options, averaging) {

  if (is_binary(averaging)) {
    roc_auc_binary(truth, estimate, options)
  }
  else if (averaging == "hand_till") {
    roc_auc_hand_till(truth, estimate, options)
  }
  else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, averaging)
    out_vec <- roc_auc_multiclass(truth, estimate, options)
    weighted.mean(out_vec, w)
  }

}

roc_auc_binary <- function(truth, estimate, options) {

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

roc_auc_multiclass <- function(truth, estimate, options) {
  res_lst <- one_vs_all_impl(roc_auc_binary, truth, estimate, options = options)
  rlang::flatten_dbl(res_lst)
}

roc_auc_hand_till <- function(truth, estimate, options) {

  lvls <- levels(truth)
  C <- length(lvls)

  multiplier <- 2 / (C * (C - 1))

  # A_hat(i | j) in the paper
  roc_auc_subset <- function(lvl1, lvl2) {
    # Subset where truth is one of the two current levels
    subset_idx <- which(truth == lvl1 | truth == lvl2)

    # Use estimate based on lvl1 being the relevant level
    # Estimate for lvl2 is just 1-lvl1 rather than the value that
    # is actually there for the multiclass case
    estimate_lvl1 <- estimate[,lvl1]

    # subset and recode truth to only have 2 levels
    truth_subset <- factor(truth[subset_idx], levels = c(lvl1, lvl2))
    estimate_subset <- estimate_lvl1[subset_idx]

    auc_val <- roc_auc_binary(truth_subset, estimate_subset, options)

    # Hand Till 2001 uses an AUC calc that is always >0.5
    # Eq 3 of https://link.springer.com/content/pdf/10.1023%2FA%3A1010920819831.pdf
    # This means their multiclass auc metric is made up of these >0.5 AUCs.
    # To be consistent, we force <0.5 AUC values to be 1-AUC which is the
    # same value that HandTill would get.
    if(auc_val < 0.5) {
      auc_val <- 1 - auc_val
    }

    auc_val
  }

  sum_val <- 0

  for(i_lvl in lvls) {

    # Double sum:
    # (sum i<j)
    cutpoint <- which(lvls == i_lvl)
    j_lvls <- lvls[-seq_len(cutpoint)]

    for(j_lvl in j_lvls) {

      # sum A_hat(i, j)
      sum_val <- sum_val +
        mean(c(roc_auc_subset(i_lvl, j_lvl), roc_auc_subset(j_lvl, i_lvl)))
    }
  }

  multiplier * sum_val
}

# PR AUC -----------------------------------------------------------------------

#' @export
#' @rdname roc_auc
pr_auc <- function(data, ...) {
  UseMethod("pr_auc")
}

class(pr_auc) <- c("prob_metric", "function")

#' @export
#' @rdname roc_auc
pr_auc.data.frame  <- function(data, truth, ...,
                               averaging = NULL,
                               na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "pr_auc",
    metric_fn = pr_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname roc_auc
pr_auc_vec <- function(truth, estimate,
                       averaging = NULL, na.rm = TRUE, ...) {

  averaging <- finalize_averaging(truth, averaging)

  pr_auc_impl <- function(truth, estimate) {
    pr_auc_averaging_impl(truth, estimate, averaging)
  }

  metric_vec_template(
    metric_impl = pr_auc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    averaging = averaging,
    cls = c("factor", "numeric"),
    averaging_override = c("binary", "macro", "macro_weighted"),
    ...
  )

}

pr_auc_averaging_impl <- function(truth, estimate, averaging) {

  if (is_binary(averaging)) {
    pr_auc_binary(truth, estimate)
  }
  else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, averaging)
    out_vec <- pr_auc_multiclass(truth, estimate)
    weighted.mean(out_vec, w)
  }

}

pr_auc_binary <- function(truth, estimate) {
  pr_list <- pr_curve_vec(truth, estimate)
  auc(pr_list[["recall"]], pr_list[["precision"]])
}

pr_auc_multiclass <- function(truth, estimate) {
  res_lst <- one_vs_all_impl(pr_auc_binary, truth, estimate)
  rlang::flatten_dbl(res_lst)
}

# Mean Log Loss ----------------------------------------------------------------

#' @export mn_log_loss
#' @rdname roc_auc
mn_log_loss <- function(data, ...) {
  UseMethod("mn_log_loss")
}

class(mn_log_loss) <- c("prob_metric", "function")

#' @export
#' @rdname roc_auc
#' @param sum A `logical`. Should the sum of the likelihood contributions be
#' returned (instead of the mean value)?
#' @importFrom rlang quo
mn_log_loss.data.frame <- function(data, truth, ...,
                                   na.rm = TRUE, sum = FALSE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "mn_log_loss",
    metric_fn = mn_log_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na.rm = na.rm,
    # Extra argument for mn_log_loss_impl()
    metric_fn_options = list(sum = sum)
  )

}

#' @rdname roc_auc
#' @importFrom stats model.matrix
#' @export
mn_log_loss_vec <- function(truth, estimate, na.rm = TRUE, sum = FALSE, ...) {

  averaging <- finalize_averaging(truth, NULL)

  # estimate here is a matrix of class prob columns
  mn_log_loss_impl <- function(truth, estimate, sum = FALSE) {
    mn_log_loss_averaging_impl(truth, estimate, averaging, sum)
  }

  metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    averaging = averaging,
    cls = c("factor", "numeric"),
    ...,
    sum = sum
  )
}

mn_log_loss_averaging_impl <- function(truth, estimate, averaging, sum = FALSE) {

  if (is_binary(averaging)) {
    mn_log_loss_binary(truth, estimate, sum)
  }
  else {
    mn_log_loss_multiclass(truth, estimate, sum)
  }

}

mn_log_loss_binary <- function(truth, estimate, sum) {
  estimate <- matrix(c(estimate, 1-estimate), ncol = 2)
  mn_log_loss_multiclass(truth, estimate, sum)
}

mn_log_loss_multiclass <- function(truth, estimate, sum) {

  y <- model.matrix(~ truth - 1)
  res <- y * estimate
  res[res <= .Machine$double.eps & res > 0] <- .Machine$double.eps
  pos_log <- function(x)
    log(x[x != 0])
  res <- -sum(unlist(apply(res, 1, pos_log)))

  if (!sum)
    res <- res / length(truth)

  res

}

# ROC Curve --------------------------------------------------------------------

#' @export
#' @rdname roc_auc
roc_curve <- function(data, ...)
  UseMethod("roc_curve")

#' @export
#' @rdname roc_auc
#' @importFrom pROC coords
#' @importFrom rlang invoke
#' @importFrom dplyr arrange as_tibble %>%
roc_curve.data.frame  <- function (data, truth, ...,
                                   options = list(),
                                   na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)
  estimate <- handle_chr_names(estimate)

  res <- dplyr::do(
    data,
    roc_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na.rm = na.rm,
      !!! list(options = options)
    )
  )

  if (".level" %in% colnames(res)) {
    res <- dplyr::group_by(res, .level, add = TRUE)
  }

  res
}

roc_curve_vec <- function(truth, estimate,
                          options = list(),
                          na.rm = TRUE,
                          ...) {

  # finalize just to see if binary/multiclass
  averaging <- finalize_averaging(truth, NULL)

  # estimate here is a matrix of class prob columns
  roc_curve_impl <- function(truth, estimate, options = list()) {
    roc_curve_averaging_impl(truth, estimate, averaging, options)
  }

  metric_vec_template(
    metric_impl = roc_curve_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    averaging = averaging,
    cls = c("factor", "numeric"),
    ...,
    options = options
  )
}

roc_curve_averaging_impl <- function(truth, estimate, averaging, options) {

  if (is_binary(averaging)) {
    roc_curve_binary(truth, estimate, options)
  }
  else {
    roc_curve_multiclass(truth, estimate, options)
  }

}

roc_curve_binary <- function(truth, estimate, options) {

  lvls <- levels(truth)

  if (getOption("yardstick.event_first", default = TRUE)) {
    lvls <- rev(lvls)
  }

  # working on a better way of doing this
  options$response <- truth
  options$predictor <- estimate
  options$levels <- lvls

  curv <- invoke(pROC::roc, options)

  if (!inherits(curv, "smooth.roc")) {
    res <- coords(
      curv,
      x = unique(c(-Inf, options$predictor, Inf)),
      input = "threshold"
    )
  }
  else {
    res <- coords(
      curv,
      x = unique(c(0, curv$specificities, 1)),
      input = "specificity"
    )
  }

  res <- dplyr::as_tibble(t(res))

  if (!inherits(curv, "smooth.roc")) {
    res <- dplyr::arrange(res, threshold)
    res <- dplyr::rename(res, .threshold = threshold)
  }
  else {
    res <- dplyr::arrange(res, specificity)
  }

  res
}

# One-VS-All approach
roc_curve_multiclass <- function(truth, estimate, options) {
  res <- one_vs_all_impl(roc_curve_binary, truth, estimate, options)

  lvls <- levels(truth)

  with_level <- function(df, lvl) {
    df$.level <- lvl
    dplyr::select(df, .level, tidyselect::everything())
  }

  res <- mapply(
    with_level,
    df = res,
    lvl = lvls,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  dplyr::bind_rows(res)
}

# PR Curve ---------------------------------------------------------------------

#' @export
#' @rdname roc_auc
pr_curve <- function(data, ...) {
  UseMethod("pr_curve")
}

#' @export
#' @rdname roc_auc
#' @importFrom stats relevel
pr_curve.data.frame <- function(data, truth, ..., na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)

  res <- dplyr::do(
    data,
    pr_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na.rm = na.rm
    )
  )

  if (".level" %in% colnames(res)) {
    res <- dplyr::group_by(res, .level, add = TRUE)
  }

  res
}

# Undecided of whether to export this or not
pr_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  # finalize just to see if binary/multiclass
  averaging <- finalize_averaging(truth, NULL)

  # estimate here is a matrix of class prob columns
  pr_curve_impl <- function(truth, estimate) {
    pr_curve_averaging_impl(truth, estimate, averaging)
  }

  metric_vec_template(
    metric_impl = pr_curve_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    averaging = averaging,
    cls = c("factor", "numeric"),
    ...
  )

}

pr_curve_averaging_impl <- function(truth, estimate, averaging) {

  if (is_binary(averaging)) {
    pr_curve_binary(truth, estimate)
  }
  else {
    pr_curve_multiclass(truth, estimate)
  }

}

pr_curve_binary <- function(truth, estimate) {

  lvls <- levels(truth)

  # Relevel if event_first = FALSE
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!getOption("yardstick.event_first")) {
    truth <- relevel(truth, lvls[2])
  }

  # quicker to convert to integer now rather than letting rcpp do it
  # 1=good, 2=bad
  truth <- as.integer(truth)

  pr_list <- pr_curve_cpp(truth, estimate)

  dplyr::tibble(!!!pr_list)
}

# One vs all approach
pr_curve_multiclass <- function(truth, estimate) {

  lvls <- levels(truth)
  other <- "..other"

  pr_curves <- rlang::new_list(n = length(lvls))

  # one vs all
  for(i in seq_along(lvls)) {

    # Recode truth into 2 levels, relevant and other
    # Pull out estimate prob column corresponding to relevant
    # Pulls by name so they dont have to be in the same order
    lvl <- lvls[i]
    truth_temp <- factor(
      x = ifelse(truth == lvl, lvl, other),
      levels = c(lvl, other)
    )
    estimate_temp <- as.numeric(estimate[, lvl])

    curve <- pr_curve_binary(truth_temp, estimate_temp)
    curve$.level = lvl
    curve <- dplyr::select(curve, .level, tidyselect::everything())

    pr_curves[[i]] <- curve
  }

  dplyr::bind_rows(pr_curves)
}

# AUC helper -------------------------------------------------------------------

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

# `...` -> estimate matrix / vector helper -------------------------------------

#' Developer helpers
#'
#' Helpers to be used alongside [metric_vec_template()] and [metric_summarizer()]
#' when creating new metrics.
#'
#' `dots_to_estimate()` is useful with class probability metrics that take
#' `...` rather than `estimate` as an argument. It constructs either a single
#' name if 1 input is provided to `...` or it constructs a quosure where the
#' expression constructs a matrix of as many columns as are provided to `...`.
#' These are eventually evaluated in the `summarise()` call in
#' [metric_summarizer()] and evaluate to either a vector or a matrix for further
#' use.
#'
#' `finalize_averaging()` is the engine for auto-selection of averaging based
#' on the type of `x`. Generally `x` is the `truth` column.
#' If `averaging` is not `NULL`, it is returned immediately
#' as no auto-selection is needed. If `x` is a:
#'
#' * `factor` - Then `"binary"` is returned if it has 2 levels, otherwise
#' `"macro"` is returned.
#'
#' * `numeric` - Then `"binary"` is returned.
#'
#' * `table` - Then `"binary"` is returned if it has 2 columns, otherwise
#' `"macro"` is returned. This is useful if you have `table` methods.
#'
#' * `matrix` - Then `"macro"` is returned.
#'
#' @name developer-helpers
#'
#' @aliases dots_to_estimate
#'
#' @export
#'
#' @inheritParams roc_auc
dots_to_estimate <- function(data, ...) {

  # Capture dots
  dot_vars <- rlang::with_handlers(
    tidyselect::vars_select(names(data), !!! enquos(...)),
    tidyselect_empty_dots = function(cnd) {
      abort("No valid variables provided to `...`.")
    }
  )

  # estimate is a matrix of the selected columns if >1 selected
  dot_nms <- lapply(dot_vars, as.name)

  if (length(dot_nms) > 1) {
    estimate <- quo(
      matrix(
        data = c(!!! dot_nms),
        ncol = !!length(dot_nms),
        dimnames = list(NULL, !!dot_vars)
      )
    )
  }
  else {
    estimate <- dot_nms[[1]]
  }


  estimate
}

# One vs all helper ------------------------------------------------------------

one_vs_all_impl <- function(metric_fn, truth, estimate, ...) {

  lvls <- levels(truth)
  other <- "..other"

  metric_lst <- rlang::new_list(n = length(lvls))

  # one vs all
  for(i in seq_along(lvls)) {

    # Recode truth into 2 levels, relevant and other
    # Pull out estimate prob column corresponding to relevant
    # Pulls by name so they dont have to be in the same order
    lvl <- lvls[i]

    truth_temp <- factor(
      x = ifelse(truth == lvl, lvl, other),
      levels = c(lvl, other)
    )

    estimate_temp <- as.numeric(estimate[, lvl])

    metric_lst[[i]] <- metric_fn(truth_temp, estimate_temp, ...)

  }

  metric_lst
}

#' @importFrom utils globalVariables
utils::globalVariables(c("estimate", "threshold", "specificity", ".level", "."))
