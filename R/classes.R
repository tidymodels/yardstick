#' Classification Metrics on Predited Classes
#'
#' Accuracy is the proportion of the data that are
#'  predicted correctly. Kappa is a similar measure but is normalized by
#'  the accuracy that would be expected by chance alone and is very useful
#'  when one or more classes have large frequency distributions.
#'
#' @inheritParams sens
#' @author Max Kuhn
#' @seealso [conf_mat()], [metrics()]
#'
#' @inherit sens return
#'
#' @references Cohen, J. (1960). "A coefficient of agreement for nominal
#'  scales". _Educational and Psychological Measurement_. 20 (1): 37–46.
#'
#' @examples
#' library(dplyr)
#' data("hpc_cv")
#'
#' # The observed and predicted classes from a single
#' # assessment set (i.e. fold)
#' fold_1 <- hpc_cv %>%
#'   filter(Resample == "Fold01")
#'
#' fold_1 %>% conf_mat(truth = obs, estimate = pred)
#'
#' fold_1 %>% accuracy(truth = obs, estimate = pred)
#'
#' fold_1 %>% kap(truth = obs, estimate = pred)
#'
#' fold_1 %>% metrics(truth = obs, estimate = pred)
#'
#' # Groups are respected so you can calculate
#' # metrics across multiple resamples at once
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   accuracy(obs, pred)
#'
#' @keywords manip
#'
#' @export
accuracy <- function(data, ...) {
  UseMethod("accuracy")
}

#' @export
#' @rdname accuracy
accuracy.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "accuracy",
    metric_fn = accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname accuracy
#' @export
accuracy.table <- function(data, ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = "accuracy",
    .estimate = accuracy_table_impl(data)
  )
}

#' @rdname accuracy
accuracy.matrix <- function(data, ...) {
  data <- as.table(data)
  accuracy.table(data)
}

#' @export
#' @rdname accuracy
accuracy_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      dnn = c("Prediction", "Truth"),
      ...
    )
    accuracy_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = accuracy_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

accuracy_table_impl <- function(data) {
  sum(diag(data)) / sum(data)
}

#' @export
#' @rdname accuracy
kap <- function(data, ...) {
  UseMethod("kap")
}

#' @export
#' @rdname accuracy
kap.data.frame  <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "kap",
    metric_fn = kap_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname accuracy
#' @export
kap.table <- function(data, ...) {
  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = "kap",
    .estimate = kap_table_impl(data)
  )
}

#' @rdname accuracy
kap.matrix <- function(data, ...) {
  data <- as.table(data)
  kap.table(data)
}

#' @export
#' @rdname accuracy
kap_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  kap_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      dnn = c("Prediction", "Truth"),
      ...
    )
    kap_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = kap_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

kap_table_impl <- function(data) {

  n <- sum(data)
  marg1 <- apply(data, 1, sum) / n
  marg2 <- apply(data, 2, sum) / n
  null_acc <- sum(marg1 * marg2)
  (accuracy_table_impl(data) - null_acc)/(1 - null_acc)

}
