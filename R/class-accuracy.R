#' Accuracy
#'
#' Accuracy is the proportion of the data that are predicted correctly.
#'
#' @family class metrics
#' @templateVar fn accuracy
#' @template return
#'
#' @section Multiclass:
#'
#' Accuracy extends naturally to multiclass scenarios. Because
#' of this, macro and micro averaging are not implemented.
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @export
#' @examples
#' library(dplyr)
#' data("two_class_example")
#' data("hpc_cv")
#'
#' # Two class
#' accuracy(two_class_example, truth, predicted)
#'
#' # Multiclass
#' # accuracy() has a natural multiclass extension
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   accuracy(obs, pred)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   accuracy(obs, pred)
accuracy <- function(data, ...) {
  UseMethod("accuracy")
}
accuracy <- new_class_metric(
  accuracy,
  direction = "maximize"
)

#' @export
#' @rdname accuracy
accuracy.data.frame <- function(data,
                                truth,
                                estimate,
                                na_rm = TRUE,
                                case_weights = NULL,
                                ...) {
  class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
accuracy.table <- function(data, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, metric_class = "accuracy")

  metric_tibbler(
    .metric = "accuracy",
    .estimator = estimator,
    .estimate = accuracy_table_impl(data)
  )
}

#' @export
accuracy.matrix <- function(data, ...) {
  data <- as.table(data)
  accuracy.table(data)
}

#' @export
#' @rdname accuracy
accuracy_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  estimator <- finalize_estimator(truth, metric_class = "accuracy")

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- handle_missings(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else {
    any_na <- detect_missings(truth, estimate, case_weights)
    if (any_na) {
      return(NA_real_)
    }
  }

  class_metric_vec_template(
    metric_impl = accuracy_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights
  )
}

# binary and multiclass case are equivalent
accuracy_impl <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()
  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  accuracy_table_impl(data)
}

accuracy_table_impl <- function(x) {
  sum(diag(x)) / sum(x)
}

