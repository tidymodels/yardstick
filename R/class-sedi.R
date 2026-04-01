#' Symmetric Extremal Dependence Index
#'
#' @description
#' Symmetric Extremal Dependence Index (SEDI) is a skill metric for
#' classification that remains reliable at extreme prevalence levels where
#' traditional metrics (TSS, MCC, Kappa) degrade. It is defined using the hit
#' rate (sensitivity) and false alarm rate (1 - specificity):
#'
#' \deqn{\text{SEDI} = \frac{\ln F - \ln H - \ln(1-F) + \ln(1-H)}
#' {\ln F + \ln H + \ln(1-F) + \ln(1-H)}}
#'
#' where \eqn{H} is sensitivity (hit rate) and \eqn{F} is the false alarm rate
#' (1 - specificity).
#'
#' @details
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{H = \text{Sensitivity} = \frac{A}{A + C}}
#'
#' \deqn{F = 1 - \text{Specificity} = \frac{B}{B + D}}
#'
#' SEDI is a metric that should be `r attr(sedi, "direction")`d. The output
#' ranges from `r metric_range(sedi)[1]` to `r metric_range(sedi)[2]`, with
#' `r metric_optimal(sedi)` indicating perfect discrimination.
#'
#' SEDI is **base-rate independent**: its value depends only on sensitivity
#' and specificity (class-conditional rates), not on prevalence. The
#' logarithmic transformation ensures the metric remains discriminating even
#' when events are extremely rare (prevalence < 2.5%), where [j_index()] (TSS)
#' converges to the hit rate alone and [mcc()] exhibits denominator
#' suppression.
#'
#' When sensitivity or specificity is exactly 0 or 1, the logarithm is
#' undefined. A small constant (`1e-9`) is used to clamp values away from
#' these boundaries.
#'
#' @section Prevalence guidance:
#' - **Prevalence >= 10%**: MCC, TSS, and SEDI all perform well.
#' - **Prevalence 2.5-10%**: SEDI preferred; MCC and TSS still usable.
#' - **Prevalence < 2.5%**: SEDI strongly recommended; MCC and TSS unreliable.
#'
#' @section Multiclass:
#'
#' Macro, micro, and macro-weighted averaging is available for this metric.
#' The default is to select macro averaging if a `truth` factor with more
#' than 2 levels is provided. Otherwise, a standard binary calculation is done.
#' See `vignette("multiclass", "yardstick")` for more information.
#'
#' For multiclass problems, SEDI is computed via one-vs-all decomposition:
#' each class is treated as a binary problem against all other classes, and a
#' per-class SEDI is calculated. Macro averaging (the default) weights all
#' classes equally, which is recommended since SEDI's log transform already
#' handles class imbalance internally. Macro-weighted averaging weights by
#' class prevalence. Micro averaging pools counts across classes before
#' computing a single SEDI value.
#'
#' @family class metrics
#' @seealso [All class metrics][class-metrics]
#' @templateVar fn sedi
#' @template event_first
#' @template return
#'
#' @inheritParams sens
#'
#' @author Simon Dedman
#'
#' @references
#'
#' Ferro, C.A.T. and Stephenson, D.B. (2011). "Extremal Dependence Indices:
#' Improved Verification Measures for Deterministic Forecasts of Rare Binary
#' Events". Weather and Forecasting. 26 (5): 699-713.
#'
#' Wunderlich, R.F., Lin, Y.-P., Anthony, J. and Petway, J.R. (2019). "Two
#' alternative evaluation metrics to replace the true skill statistic in the
#' assessment of species distribution models". Nature Conservation. 35: 97-116.
#'
#' @template examples-class
#'
#' @export
sedi <- function(data, ...) {
  UseMethod("sedi")
}
sedi <- new_class_metric(
  sedi,
  direction = "maximize",
  range = c(-1, 1)
)

#' @rdname sedi
#' @export
sedi.data.frame <- function(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  class_metric_summarizer(
    name = "sedi",
    fn = sedi_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level
  )
}

#' @export
sedi.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "sedi",
    .estimator = estimator,
    .estimate = sedi_table_impl(data, estimator, event_level)
  )
}

#' @export
sedi.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  sedi.table(data, estimator, event_level)
}

#' @rdname sedi
#' @export
sedi_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_bool(na_rm)
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, estimator)

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  sedi_table_impl(data, estimator, event_level)
}

sedi_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    sedi_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- sedi_multiclass(data, estimator)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

sedi_binary <- function(data, event_level) {
  sens <- sens_binary(data, event_level)
  spec <- spec_binary(data, event_level)

  small <- 1e-9
  H <- max(min(sens, 1 - small), small)
  Fa <- max(min(1 - spec, 1 - small), small)

  (log(Fa) - log(H) - log(1 - Fa) + log(1 - H)) /
    (log(Fa) + log(H) + log(1 - Fa) + log(1 - H))
}

sedi_multiclass <- function(data, estimator) {
  n <- sum(data)
  tp <- diag(data)
  tpfp <- rowSums(data) # predicted as class k
  tpfn <- colSums(data) # actual class k
  fn <- tpfn - tp
  fp <- tpfp - tp
  tn <- n - (tpfp + tpfn - tp)

  if (is_micro(estimator)) {
    # Pool counts across classes, then compute single SEDI
    H <- sum(tp) / sum(tp + fn)
    Fa <- sum(fp) / sum(fp + tn)

    small <- 1e-9
    H <- max(min(H, 1 - small), small)
    Fa <- max(min(Fa, 1 - small), small)

    return(
      (log(Fa) - log(H) - log(1 - Fa) + log(1 - H)) /
        (log(Fa) + log(H) + log(1 - Fa) + log(1 - H))
    )
  }

  # Per-class SEDI for macro / macro_weighted
  H_vec <- tp / (tp + fn)
  Fa_vec <- fp / (fp + tn)

  # Handle undefined (class with no actual positives or no actual negatives)
  undefined <- (tp + fn) <= 0 | (fp + tn) <= 0
  if (any(undefined)) {
    H_vec[undefined] <- NA_real_
    Fa_vec[undefined] <- NA_real_
  }

  small <- 1e-9
  H_vec <- pmax(pmin(H_vec, 1 - small), small)
  Fa_vec <- pmax(pmin(Fa_vec, 1 - small), small)

  (log(Fa_vec) - log(H_vec) - log(1 - Fa_vec) + log(1 - H_vec)) /
    (log(Fa_vec) + log(H_vec) + log(1 - Fa_vec) + log(1 - H_vec))
}
