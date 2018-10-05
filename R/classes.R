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
accuracy.data.frame <- function(data, truth, estimate, averaging = "binary",
                                na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = construct_name("accuracy", averaging),
    metric_fn = accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname accuracy
#' @export
accuracy.table <- function(data, averaging = "binary", ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("accuracy", averaging),
    .estimate = accuracy_table_impl(data, averaging)
  )
}

#' @rdname accuracy
accuracy.matrix <- function(data, averaging = "binary", ...) {
  data <- as.table(data)
  accuracy.table(data, averaging)
}

#' @export
#' @rdname accuracy
accuracy_vec <- function(truth, estimate, averaging = "binary", na.rm = TRUE, ...) {

  accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    accuracy_table_impl(xtab, averaging)

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

accuracy_table_impl <- function(data, averaging) {

  if(is_binary(averaging)) {

    accuracy_binary(data)

  # multiclass accuracy has a well defined case here
  # (TP + TN) / N
  # same as the binary formula
  } else if (averaging == "exact") {

    accuracy_binary(data)

  } else {

    w <- get_weights(data, averaging)
    out_vec <- accuracy_multiclass(data, averaging)
    weighted.mean(out_vec, w)

  }

}

accuracy_binary <- function(data) {
  sum(diag(data)) / sum(data)
}

accuracy_multiclass <- function(data, averaging) {

  n <- rep(sum(data), times = nrow(data))
  tp <- diag(data)
  tn <- n - (rowSums(data) + colSums(data) - tp)

  if(is_micro(averaging)) {
    tp <- sum(tp)
    tn <- sum(tn)
    n <- sum(n)
  }

  (tp + tn) / n
}

#' @export
#' @rdname accuracy
kap <- function(data, ...) {
  UseMethod("kap")
}

#' @export
#' @rdname accuracy
kap.data.frame  <- function(data, truth, estimate, averaging = "binary",
                            na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = construct_name("kap", averaging),
    metric_fn = kap_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname accuracy
#' @export
kap.table <- function(data, averaging = "binary", ...) {
  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("kap", averaging),
    .estimate = kap_table_impl(data, averaging)
  )
}

#' @rdname accuracy
kap.matrix <- function(data, averaging = "binary", ...) {
  data <- as.table(data)
  kap.table(data, averaging)
}

#' @export
#' @rdname accuracy
kap_vec <- function(truth, estimate, averaging = "binary", na.rm = TRUE, ...) {

  kap_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    kap_table_impl(xtab, averaging)

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

kap_table_impl <- function(data, averaging) {

  if(is_binary(averaging)) {

    kap_binary(data)

    # multiclass kappa has a well defined case here
  } else if (averaging == "cohen") {

    kap_binary(data)

  } else {

    w <- get_weights(data, averaging)
    out_vec <- kap_multiclass(data, averaging)
    weighted.mean(out_vec, w)

  }
}

kap_binary <- function(data) {

  n <- sum(data)

  .row_sums <- rowSums(data)
  .col_sums <- colSums(data)

  expected_acc <- sum( (.row_sums * .col_sums) / n ) / n

  obs_acc <- accuracy_binary(data)

  (obs_acc - expected_acc) / (1 - expected_acc)
}

kap_multiclass <- function(data, averaging) {

  n <- rep(sum(data), times = nrow(data))

  relevant_row <- rowSums(data)
  other_row <- sum(relevant_row) - relevant_row

  relevant_col <- colSums(data)
  other_col <- sum(relevant_col) - relevant_col

  if(is_micro(averaging)) {
    relevant_row <- sum(relevant_row)
    relevant_col <- sum(relevant_col)
    other_row <- sum(other_row)
    other_col <- sum(other_col)
    n <- sum(n)
  }

  expected_acc <- (relevant_row * relevant_col + other_row * other_col) / n / n

  obs_acc <- accuracy_multiclass(data, averaging)

  (obs_acc - expected_acc) / (1 - expected_acc)
}
