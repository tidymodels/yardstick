#' Confusion Matrix for Categorical Data
#'
#' Calculates a cross-tabulation of observed and predicted classes.
#'
#' For [conf_mat()] objects, a `broom` `tidy()` method has been created
#' that collapses the cell counts by cell into a data frame for
#' easy manipulation.
#'
#' There is also a `summary()` method that computes various classification
#' metrics at once. See [summary.conf_mat()]
#'
#' The function requires that the factors have exactly the same levels.
#'
#' @aliases conf_mat.table conf_mat.default conf_mat
#'
#' @inheritParams sens
#'
#' @param data A data frame or a [base::table()].
#'
#' @param dnn A character vector of dimnames for the table.
#'
#' @param ... Options to pass to [base::table()] (not including
#'  `dnn`). This argument is not currently used for the `tidy`
#'  method.
#'
#' @return
#' `conf_mat()` produces an object with class `conf_mat`. This contains the
#' table and other objects. `tidy.conf_mat()` generates a tibble with columns
#' `name` (the cell identifier) and `value` (the cell count).
#'
#' When used on a grouped data frame, `conf_mat()` returns a tibble containing
#' columns for the groups along with `conf_mat`, a list-column
#' where each element is a `conf_mat` object.
#'
#' @seealso
#'
#' [summary.conf_mat()] for computing a large number of metrics from one
#' confusion matrix.
#'
#' @examples
#' library(dplyr)
#' data("hpc_cv")
#'
#' # The confusion matrix from a single assessment set (i.e. fold)
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   conf_mat(obs, pred)
#'
#' # Now compute the average confusion matrix across all folds in
#' # terms of the proportion of the data contained in each cell.
#' # First get the raw cell counts per fold using the `tidy` method
#' library(purrr)
#' library(tidyr)
#'
#' cells_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   conf_mat(obs, pred) %>%
#'   mutate(tidied = map(conf_mat, tidy)) %>%
#'   unnest(tidied)
#'
#' # Get the totals per resample
#' counts_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   summarize(total = n()) %>%
#'   left_join(cells_per_resample, by = "Resample") %>%
#'   # Compute the proportions
#'   mutate(prop = value/total) %>%
#'   group_by(name) %>%
#'   # Average
#'   summarize(prop = mean(prop))
#'
#' counts_per_resample
#'
#' # Now reshape these into a matrix
#' mean_cmat <- matrix(counts_per_resample$prop, byrow = TRUE, ncol = 4)
#' rownames(mean_cmat) <- levels(hpc_cv$obs)
#' colnames(mean_cmat) <- levels(hpc_cv$obs)
#'
#' round(mean_cmat, 3)
#' @export
conf_mat <- function(data, ...) {
  UseMethod("conf_mat")
}

#' @export
#' @rdname conf_mat
conf_mat.data.frame <- function(data, truth, estimate,
                                dnn = c("Prediction", "Truth"), ...) {

  vars <-
    factor_select(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      ...
    )

  xtab <- vec2table(
    truth = data[[vars$truth]],
    estimate = data[[vars$estimate]],
    dnn = dnn,
    ...
  )
  conf_mat.table(xtab, ...)

}

#' @export
conf_mat.grouped_df <- function(data, truth, estimate,
                                dnn = c("Prediction", "Truth"), ...) {

  vars <-
    factor_select(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      ...
    )

  truth <- as.name(vars$truth)
  estimate <- as.name(vars$estimate)

  dplyr::summarise(
    data,
    conf_mat = {
      xtab <- vec2table(
        truth = !! truth,
        estimate = !! estimate,
        dnn = dnn,
        ...
      )
      list(conf_mat.table(xtab, ...))
    }
  )

}

#' @export
conf_mat.table <- function(data, ...) {

  if (length(dim(data)) != 2)
    stop("`data` must have two dimensions", call. = FALSE)

  check_table(data)

  class_lev <- rownames(data)
  num_lev <- length(class_lev)

  if (num_lev < 2)
    stop("there must be at least 2 factors levels in the `data`",
         call. = FALSE)

  structure(list(table = data,
                 dots = list(...)),
            class = "conf_mat")
}

#' @export
conf_mat.matrix <- function(data, ...) {

  data <- as.table(data)
  conf_mat.table(data, ...)

}


#' @export
print.conf_mat <- function(x, ...)
  print(x$table)

#' @export
#' @rdname conf_mat
tidy.conf_mat <- function(x, ...) {
  y <- flatten(x$table)
  dplyr::tibble(
    name = names(y),
    value = unname(y)
  )
}


#' Summary Statistics for Confusion Matrices
#'
#' Various statistical summaries of confusion matrices are
#' produced and returned in a tibble. These include those shown in the help
#' pages for [sens()], [recall()], and [accuracy()], among others.
#'
#' @template event_first
#'
#' @inheritParams sens
#'
#' @param object An object of class [conf_mat()].
#'
#' @param prevalence A number in `(0, 1)` for the prevalence (i.e.
#'  prior) of the event. If left to the default, the data are used
#'  to derive this value.
#'
#' @param beta A numeric value used to weight precision and
#'  recall for [f_meas()].
#'
#' @param ... Not currently used.
#'
#' @return
#'
#' A tibble containing various classification metrics.
#'
#' @seealso
#'
#' [conf_mat()]
#'
#' @examples
#' data("two_class_example")
#'
#' cmat <- conf_mat(two_class_example, truth = "truth", estimate = "predicted")
#' summary(cmat)
#' summary(cmat, prevalence = 0.70)
#'
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#' data("hpc_cv")
#'
#' # Compute statistics per resample then summarize
#' all_metrics <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   conf_mat(obs, pred) %>%
#'   mutate(summary_tbl = map(conf_mat, summary)) %>%
#'   unnest(summary_tbl)
#'
#' all_metrics %>%
#'   group_by(.metric) %>%
#'   summarise(
#'     mean = mean(.estimate, na.rm = TRUE),
#'     sd = sd(.estimate, na.rm = TRUE)
#'   )
#'
#'
#' @export
#' @importFrom dplyr bind_rows
summary.conf_mat <- function(object,
                             prevalence = NULL,
                             beta = 1,
                             estimator = NULL,
                             ...) {

  xtab <- object$table

  stats <- bind_rows(
    # known multiclass extension
    accuracy(xtab),
    # known multiclass extension
    kap(xtab),
    sens(xtab, estimator = estimator),
    spec(xtab, estimator = estimator),
    ppv(xtab, prevalence = prevalence, estimator = estimator),
    npv(xtab, prevalence = prevalence, estimator = estimator),
    # known multiclass extension
    mcc(xtab),
    j_index(xtab, estimator = estimator),
    bal_accuracy(xtab, estimator = estimator),
    detection_prevalence(xtab, estimator = estimator),
    precision(xtab, estimator = estimator),
    recall(xtab, estimator = estimator),
    f_meas(xtab, beta = beta, estimator = estimator)
  )

  stats
}
