#' Area under the receiver operator curve
#'
#' `roc_auc()` is a metric that computes the area under the ROC curve. See
#' [roc_curve()] for the full curve.
#'
#' For most methods, `roc_auc()` defaults to allowing `pROC::roc()` control
#' the direction of the computation, but allows you to control this by passing
#' `options = list(direction = "<")` or any other allowed direction value.
#' However, the Hand, Till (2001) method assumes that the individual AUCs are
#' all above `0.5`, so if an AUC value below `0.5` is computed, then `1` is
#' subtracted from it to get the correct result. When not using the Hand, Till
#' method, pROC advises setting the `direction` when doing resampling so that
#' the AUC values are not biased upwards.
#'
#' Generally, an ROC AUC value is between `0.5` and `1`, with `1` being a
#' perfect prediction model. If your value is between `0` and `0.5`, then
#' this implies that you have meaningful information in your model, but it
#' is being applied incorrectly because doing the opposite of what the model
#' predicts would result in an AUC `>0.5`.
#'
#' @family class probability metrics
#' @templateVar metric_fn roc_auc
#' @template return
#' @template event_first
#'
#' @section Multiclass:
#' The default multiclass method for computing `roc_auc()` is to use the
#' method from Hand, Till, (2001). Unlike macro-averaging, this method is
#' insensitive to class distributions like the binary ROC AUC case.
#'
#' Macro and macro-weighted averaging are still provided, even though they are
#' not the default. In fact, macro-weighted averaging corresponds to the same
#' definition of multiclass AUC given by Provost and Domingos (2001).
#'
#' @inheritParams pr_auc
#'
#' @param options A `list` of named options to pass to [pROC::roc()]
#' such as `direction` or `smooth`. These options should not include `response`,
#' `predictor`, `levels`, or `quiet`.
#'
#' @param estimator One of `"binary"`, `"hand_till"`, `"macro"`, or
#' `"macro_weighted"` to specify the type of averaging to be done. `"binary"`
#' is only relevant for the two class case. The others are general methods for
#' calculating multiclass metrics. The default will automatically choose
#' `"binary"` or `"hand_till"` based on `truth`.
#'
#' @references
#'
#' Hand, Till (2001). "A Simple Generalisation of the Area Under the
#' ROC Curve for Multiple Class Classification Problems". _Machine Learning_.
#' Vol 45, Iss 2, pp 171-186.
#'
#' Fawcett (2005). "An introduction to ROC analysis". _Pattern Recognition
#' Letters_. 27 (2006), pp 861-874.
#'
#' Provost, F., Domingos, P., 2001. "Well-trained PETs: Improving probability
#' estimation trees", CeDER Working Paper #IS-00-04, Stern School of Business,
#' New York University, NY, NY 10012.
#'
#' @seealso
#'
#' [roc_curve()] for computing the full ROC curve.
#'
#' @author Max Kuhn
#'
#' @template examples-binary-prob
#' @template examples-multiclass-prob
#' @examples
#' # ---------------------------------------------------------------------------
#' # Options for `pROC::roc()`
#'
#' # Pass options via a named list and not through `...`!
#' roc_auc(
#'   two_class_example,
#'   truth = truth,
#'   Class1,
#'   options = list(smooth = TRUE)
#' )
#'
#' @export
roc_auc <- function(data, ...) {
  UseMethod("roc_auc")
}

class(roc_auc) <- c("prob_metric", "function")
attr(roc_auc, "direction") <- "maximize"

#' @export
#' @rdname roc_auc
roc_auc.data.frame  <- function(data, truth, ..., options = list(),
                                estimator = NULL, na_rm = TRUE) {


  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "roc_auc",
    metric_fn = roc_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = estimator,
    na_rm = na_rm,
    ... = ...,
    metric_fn_options = list(options = options)
  )

}

#' @rdname roc_auc
#' @export
#' @importFrom rlang call2
#' @importFrom pROC roc auc
roc_auc_vec <- function(truth, estimate, options = list(),
                        estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator, "roc_auc")

  roc_auc_impl <- function(truth, estimate) {
    roc_auc_estimator_impl(truth, estimate, options, estimator)
  }

  metric_vec_template(
    metric_impl = roc_auc_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    ...
  )
}

roc_auc_estimator_impl <- function(truth, estimate, options, estimator) {

  if (is_binary(estimator)) {
    roc_auc_binary(truth, estimate, options)
  }
  else if (estimator == "hand_till") {
    roc_auc_hand_till(truth, estimate, options)
  }
  else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, estimator)
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

  args <- quos(response = truth, predictor = estimate, levels = lvl, quiet = TRUE)

  pROC_auc <- eval_tidy(call2("auc", !!! args, !!! options, .ns = "pROC"))

  as.numeric(pROC_auc)

}

roc_auc_multiclass <- function(truth, estimate, options) {
  res_lst <- one_vs_all_impl(roc_auc_binary, truth, estimate, options = options)
  rlang::flatten_dbl(res_lst)
}

roc_auc_hand_till <- function(truth, estimate, options) {

  # Hand Till method ignores yardstick.event_first (like macro-average)
  old_opt <- getOption("yardstick.event_first", TRUE)
  options(yardstick.event_first = TRUE)
  on.exit(options(yardstick.event_first = old_opt))

  lvls <- levels(truth)
  C <- length(lvls)

  multiplier <- 2 / (C * (C - 1))

  # We want to reference the levels by name in the function below, so we
  # force the column names to be the same as the levels
  # (and assume the prob matrix columns are given in the same
  # order as the levels of `truth`)
  colnames(estimate) <- lvls

  # A_hat(i | j) in the paper
  roc_auc_subset <- function(lvl1, lvl2) {
    # Subset where truth is one of the two current levels
    subset_idx <- which(truth == lvl1 | truth == lvl2)

    # Use estimate based on lvl1 being the relevant level
    # Estimate for lvl2 is just 1-lvl1 rather than the value that
    # is actually there for the multiclass case
    estimate_lvl1 <- estimate[, lvl1, drop = TRUE]

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
