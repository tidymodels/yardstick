#' Weighted Kappa
#'
#' `kap_weighted` is a variant of \code{\link{kap}} with weights depending on
#' the distance of the estimate from the true value. To allow for calculation of
#' that distance, `truth` and `estimate` should be ordered factors. Note that
#' `kap_weighted` is identical to \code{\link{kap}} for two classes.
#'
#' @family class metrics
#' @templateVar metric_fn kap_weighted
#' @template return
#'
#' @section Multiclass:
#'
#'   Weighted kappa extends naturally to multiclass scenarios. Because of this,
#'   macro and micro averaging are not implemented.
#'
#' @inheritParams sens
#'
#' @author Jon Harmon
#'
#' @references Cohen, J. (1968). "Weighed kappa: Nominal scale agreement with
#'   provision for scaled disagreement or partial credit". _Psychological
#'   Bulletin_. 70 (4): 213-220.
#'
#' @template examples-class
#'
#' @export
kap_weighted <- function(data, ...) {
  UseMethod("kap_weighted")
}

class(kap_weighted) <- c("class_metric", "function")
attr(kap_weighted, "direction") <- "maximize"

#' @export
#' @rdname kap_weighted
kap_weighted.data.frame  <- function(data, truth, estimate, weight = "linear",
                            na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "kap_weighted",
    metric_fn = kap_weighted_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    # do not pass dots through (could allow averaging to be set. unwanted!)
    # Extra argument for kap_weighted_impl()
    metric_fn_options = list(weight = weight)
  )

}

#' @export
kap_weighted.table <- function(data, weight, ...) {
  check_table(data)
  metric_tibbler(
    .metric = "kap_weighted",
    .estimator = finalize_estimator(data, metric_class = "kap_weighted"),
    .estimate = kap_weighted_table_impl(data, weight)
  )
}

#' @export
kap_weighted.matrix <- function(data, weight, ...) {
  data <- as.table(data)
  kap_weighted.table(data, weight)
}

#' @export
#' @rdname kap_weighted
kap_weighted_vec <- function(truth, estimate, weight = "linear", na_rm = TRUE,
                             ...) {

  estimator <- finalize_estimator(truth, metric_class = "kap_weighted")

  kap_weighted_impl <- function(truth, estimate, weight) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    kap_weighted_table_impl(xtab, weight)

  }

  metric_vec_template(
    metric_impl = kap_weighted_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("ordered", "factor"),
    weight = weight,
    ...
  )

}

kap_weighted_table_impl <- function(data, weight) {
  kap_weighted_binary(data, weight)
}

kap_weighted_binary <- function(data, weight) {

  n <- sum(data)
  n_class <- dim(data)[[1]]
  .row_sums <- rowSums(data)
  .col_sums <- colSums(data)

  weight_power <- switch(
    weight,
    linear = 1L,
    quadratic = 2L
  )

  if (is.null(weight_power)) {
    stop("`weight` must be one of: ",
         "linear, quadratic", call. = FALSE)
  }

  weight_power <- ifelse(weight == "linear", 1L, 2L)
  weight_matrix <- abs(outer(1:n_class, 1:n_class, "-"))^weight_power

  expected_matrix <- outer(.row_sums, .col_sums)/n

  N_disagree <- sum(data*weight_matrix)
  N_disagree_chance <- sum(expected_matrix*weight_matrix)

  1-N_disagree/N_disagree_chance

}
