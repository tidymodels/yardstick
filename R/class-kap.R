#' Kappa
#'
#' Kappa is a similar measure to [accuracy()], but is normalized by
#' the accuracy that would be expected by chance alone and is very useful
#' when one or more classes have large frequency distributions.
#'
#' @family class metrics
#' @templateVar metric_fn kap
#' @template return
#'
#' @section Multiclass:
#'
#' Kappa extends naturally to multiclass scenarios. Because
#' of this, macro and micro averaging are not implemented.
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @references Cohen, J. (1960). "A coefficient of agreement for nominal
#'  scales". _Educational and Psychological Measurement_. 20 (1): 37-46.
#'
#' @template examples-class
#'
#' @export
kap <- function(data, ...) {
  UseMethod("kap")
}

class(kap) <- c("class_metric", "function")

#' @export
#' @rdname kap
kap.data.frame  <- function(data, truth, estimate,
                            na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "kap",
    metric_fn = kap_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm
    # do not pass dots through (could allow averaging to be set. unwanted!)
  )

}

#' @export
kap.table <- function(data, ...) {
  check_table(data)
  metric_tibbler(
    .metric = "kap",
    .estimator = finalize_estimator(data, metric_class = "kap"),
    .estimate = kap_table_impl(data)
  )
}

#' @export
kap.matrix <- function(data, ...) {
  data <- as.table(data)
  kap.table(data)
}

#' @export
#' @rdname kap
kap_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "kap")

  kap_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    kap_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = kap_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

kap_table_impl <- function(data) {
  kap_binary(data)
}

kap_binary <- function(data) {

  n <- sum(data)

  .row_sums <- rowSums(data)
  .col_sums <- colSums(data)

  expected_acc <- sum( (.row_sums * .col_sums) / n ) / n

  obs_acc <- accuracy_binary(data)

  (obs_acc - expected_acc) / (1 - expected_acc)
}
