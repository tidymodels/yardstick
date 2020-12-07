# TODO:
# - Templates in roxygen2 for survival?
# - New metric set helper? Multiple of them?

#' Concordance index
#'
#' Calculates the concordance index using [survival::concordance()].
#'
#' @family survival metrics
#'
#' @inheritParams rmse
#'
#' @param truth The column name for the true results (that is a
#'   `Surv` object). This can be a bare column name that evaluates to a
#'   `Surv` column in `data`, or can be an expression like
#'   `Surv(<time>, <event>)` to construct the `Surv` object on the fly,
#'   where `<time>` and `<event>` would be replaced with the corresponding
#'   columns in `data`.
#'
#' @param estimate The column name for the predicted results
#'   (that is `numeric`). This should be a bare column name that evaluates
#'   to the linear predictor
#'
#' @author Max Kuhn
#'
#' @template examples-numeric
#'
#' @export
c_index <- function(data, ...) {
  UseMethod("c_index")
}
# TODO: New survival metric?
# c_index <- new_survival_metric(
#   c_index,
#   direction = "maximize"
# )

#' @export
#' @rdname c_index
c_index.data.frame <- function(data,
                               truth,
                               estimate,
                               na_rm = TRUE,
                               ...) {
  metric_summarizer(
    metric_nm = "c_index",
    metric_fn = c_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

#' @export
#' @rdname c_index
c_index_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  c_index_impl <- function(truth, estimate) {
    data <- data.frame(truth = truth, estimate = estimate)
    conc <- survival::concordance(truth ~ estimate, data = data)
    conc$concordance
  }

  metric_vec_template(
    metric_impl = c_index_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c("Surv", "numeric"),
    ...
  )
}
