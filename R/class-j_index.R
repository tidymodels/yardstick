#' J-index
#'
#' @family class metrics
#' @templateVar metric_fn j_index
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
j_index <- function(data, estimator = NULL, ...) {
  UseMethod("j_index")
}

class(j_index) <- c("class_metric", "function")

#' @rdname j_index
#' @export
j_index.data.frame <- function(data, truth, estimate,
                               estimator = NULL,
                               na.rm = TRUE,
                               ...) {

  metric_summarizer(
    metric_nm = "j_index",
    metric_fn = j_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
j_index.table <- function(data, estimator = NULL, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "j_index",
    .estimator = estimator,
    .estimate = j_index_table_impl(data, estimator)
  )

}

#' @export
j_index.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  j_index.table(data, estimator)

}

#' @rdname j_index
#' @export
j_index_vec <- function(truth, estimate, estimator = NULL,
                        na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  j_index_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    j_index_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = j_index_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    estimator = estimator,
    ...
  )

}

j_index_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    j_index_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- j_index_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

j_index_binary <- function(data) {
  # sens + spec - 1
  recall_binary(data) + spec_binary(data) - 1
}

j_index_multiclass <- function(data, estimator) {
  recall_multiclass(data, estimator) + spec_multiclass(data, estimator) - 1
}
