#' Mean error
#'
#' @description
#' Calculate the mean error (also known as mean signed deviation).
#' It can be used as a measure of the `estimate`'s bias.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn me
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Thomas Bierhance
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' set.seed(1234)
#' size <- 100
#' times <- 10
#'
#' # create 10 resamples
#' solubility_resampled <- bind_rows(
#'   replicate(
#'     n = times,
#'     expr = sample_n(solubility_test, size, replace = TRUE),
#'     simplify = FALSE
#'   ),
#'   .id = "resample"
#' )
#'
#' # Compute the metric by group
#' metric_results <- solubility_resampled %>%
#'   group_by(resample) %>%
#'   me(solubility, prediction)
#'
#' metric_results
#'
#' # Resampled mean estimate
#' metric_results %>%
#'   summarise(avg_estimate = mean(.estimate))
me <- function(data, ...) {
  UseMethod("me")
}
me <- new_numeric_metric(
  me,
  direction = "zero"
)

#' @rdname me
#' @export
me.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "me",
    metric_fn = me_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname me
me_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  me_impl <- function(truth, estimate) {
    mean( (truth - estimate) )
  }

  metric_vec_template(
    metric_impl = me_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
