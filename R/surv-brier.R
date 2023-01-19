#' Brier score for censored data models
#'
#' Compute the Brier score for a censored data model.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival
#' @template return
#' @details
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @section Multiclass:
#' Brier scores can be computed in the same way for any number of classes.
#' Because of this, no averaging types are supported.
#'
#' @inheritParams pr_auc
#' @param .time A vector of time points.
#'
#' @author Emil Hvitfeldt
#'
#' @export
brier_survival <- function(data, ...) {
  UseMethod("brier_survival")
}
brier_survival <- new_dynamic_survival_metric(
  brier_survival,
  direction = "minimize"
)

#' @rdname brier_survival
#' @export
brier_survival.data.frame <- function(data,
                                  truth,
                                  estimate,
                                  .time,
                                  na_rm = TRUE,
                                  case_weights = NULL,
                                  ...) {
  dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    .time = .time,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_survival
brier_survival_vec <- function(truth,
                           estimate,
                           .time,
                           na_rm = TRUE,
                           case_weights = NULL,
                           ...) {
  check_survival_dynamic_metric(truth, estimate, case_weights, .time)

  if (na_rm) {
    estimate_index <- seq_along(estimate)
    result <- yardstick_remove_missing(truth, estimate_index, case_weights)

    truth <- result$truth
    estimate <- estimate[result$estimate]
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, seq_along(estimate), case_weights)) {
    return(NA_real_)
  }

  brier_survival_impl(truth, estimate, case_weights, .time)
}

brier_survival_impl <- function(truth, estimate, case_weights, .time) {
  data <- dplyr::tibble(truth, estimate)
  data <- tidyr::unnest(data, estimate)

  res <- numeric(length(.time))

  for (i in seq_along(.time)) {
    .time_loc <- .time[i] == data[[".time"]]

    res[i] <- calc_rcbs(
      data[["truth"]][.time_loc],
      data[[".pred_survival"]][.time_loc],
      .time[i]
    )
  }

  res
}

calc_rcbs <- function(surv, pred_val, .t) {
  surv_time <- surv[, "time"]
  surv_status <- surv[, "status"]

  time_order <- order(surv_time)
  surv_time <- surv_time[time_order]
  surv_status <- surv_status[time_order]
  pred_val <- pred_val[time_order]
  surv <- survival::Surv(surv_time, surv_status)

  censor_dist <- censor_probs(surv)

  ipcw_dot_time <- get_single_censor_prob(.t, censor_dist)
  ipcw_dot_time <- 1 - ipcw_dot_time

  category_1 <- surv_time < .t & surv_status == 1
  category_2 <- surv_time >= .t

  ipcw_vals <- vapply(
    surv_time,
    get_single_censor_prob,
    probs = censor_dist,
    FUN.VALUE = numeric(1)
  )
  ipcw_vals <- 1 - ipcw_vals

  # (0 - pred_val) ^ 2 == pred_val ^ 2
  category_1_vals <- pred_val ^ 2 / ipcw_vals
  category_2_vals <- (1 - pred_val) ^ 2 / ipcw_dot_time

  category_1_sum <- sum(category_1_vals[category_1])
  category_2_sum <- sum(category_2_vals[category_2])

  (category_1_sum + category_2_sum) / length(pred_val)
}
