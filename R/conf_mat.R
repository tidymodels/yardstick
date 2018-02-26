#' Confusion Matrix for Categorical Data
#'
#' Calculates a cross-tabulation of observed and predicted
#'  classes.
#'
#' The function requires that the factors have exactly the same
#'  levels.

#' @inheritParams sens
#' @aliases conf_mat.table conf_mat.default conf_mat
#' @param data A data frame or a [base::table()].
#' @param dnn a character vector of dimnames for the table
#' @param ... Options to pass to [base::table()] (not including
#'  `dnn`). This argument is not currently used for the `tidy`
#'  method.
#' @return `conf_mat` produces a object with class `conf_mat`.
#'  This contains the table and other objects. `tidy.conf_mat`
#'  generates a tibble with columns `name` (the cell identifier) and
#'  `value` (the cell count).
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
#' cells_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   do(tidy(conf_mat(., obs, pred)))
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
conf_mat <- function(data, ...)
  UseMethod("conf_mat")

#' @export
#' @rdname conf_mat
conf_mat.data.frame  <-
  function(data,
           truth,
           estimate,
           dnn = c("Prediction", "Truth"),
           ...) {
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
    conf_mat(xtab, ...)
  }

#' @rdname conf_mat
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
print.conf_mat <- function(x, ...)
  print(x$table)

#' Tidy Representation of Confusion Matrices
#'
#' For [conf_mat()] objects, the `tidy` method collapses the cell
#'  counts by cell into a data frame for each manipulation.
#' @param x A object of class [conf_mat()].
#' @importFrom tibble tibble
#' @importFrom broom tidy
#' @rdname conf_mat
#' @export
tidy.conf_mat <- function(x, ...) {
  y <- flatten(x$table)
  tibble(name = names(y),
         value = unname(y))
}


#' Summary Statistics for Confusion Matrices
#'
#' Various statistical summaries of confusion matrices are
#'  produced and returned in a easily used format. These potentially
#'  include those shown in the help pages for [sens()], [recall()],
#'  and [accuracy()].
#'
#' @details
#' There is no common convention on which factor level should
#'  automatically be considered the "event" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' @param object An object of class [conf_mat()].
#' @param prevalence A number in `(0, 1)` for the prevalence (i.e.
#'  prior) of the event. If left to the default, the data are used
#'  to derive this value.
#' @param beta A numeric value used to weight precision and
#'  recall for [f_meas()].
#' @param wide A single logical value: should there be one row and
#'  columns for each statistic (`wide = TRUE`) or a column for the
#'  statistic name (`name`) and the estimate (`value`).
#' @param ... Not currently used.
#' @return A tibble. Note that if the argument `prevalence` was
#'  used, the value reported in the tibble reflects the argument
#'  value and not the observed rate of events.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom tidyr gather
#' @examples
#' data("two_class_example")
#'
#' cmat <- conf_mat(two_class_example, truth = "truth", estimate = "predicted")
#' summary(cmat, wide = TRUE)
#' summary(cmat, wide = TRUE, prevalence = 0.70)
#'
#' library(dplyr)
#' data("hpc_cv")
#'
#' # Compute statistics per resample then summarize
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   do(summary(conf_mat(., truth = "obs", estimate = "pred"))) %>%
#'   group_by(name) %>%
#'   summarize(mean = mean(value, na.rm = TRUE),
#'             sd = sd(value, na.rm = TRUE))
#'
summary.conf_mat <- function(object, prevalence = NULL,
                             beta = 1, wide = FALSE, ...) {
  xtab <- object$table
  stats <-
    tibble(accuracy = accuracy(xtab),
           kappa = kap(xtab))

  if (nrow(xtab) == 2) {
    positive <- pos_val(xtab)

    if (is.null(prevalence))
      prevalence <- sum(xtab[, positive]) / sum(xtab)

    stats_2class <-
      tibble(
        sens = sens(xtab),
        spec = spec(xtab),
        prevalence = prevalence,
        ppv = ppv(xtab, prevalence = prevalence),
        npv = npv(xtab, prevalence = prevalence),
        mcc = mcc(xtab),
        j_index = j_index(xtab),
        balanced_accuracy = bal_accuracy(xtab),
        detection_prevalence = bal_accuracy(xtab),
        precision = precision(xtab),
        recall = recall(xtab),
        F1 = f_meas(xtab, beta = beta)
      )
    stats <- bind_cols(stats, stats_2class)
  }
  if(!wide)
    stats <- gather(stats, name, value)
  stats
}

#' @importFrom utils globalVariables
utils::globalVariables(c("name", "value"))
