#' Confusion Matrix for Categorical Data
#'
#' Calculates a cross-tabulation of observed and predicted
#'  classes.
#'
#' The function requires that the factors have exactly the same
#'  levels.

#'
#' @aliases conf_mat.table conf_mat.default conf_mat
#' @param data A data frame or a [base::table()].
#' @param truth A string corresponding to the column in the data
#'  frame containing the factor of classes to be used as the true
#'  results
#' @param estimate A string corresponding to the column in the
#'  data frame containing the factor of predicted classes.
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
#'   conf_mat(truth = "obs", estimate = "pred")
#' 
#' # Now compute the average confusion matrix across all folds in
#' # terms of the proportion of the data contained in each cell. 
#' # First get the raw cell counts per fold using the `tidy` method
#' cells_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   do(tidy(conf_mat(., truth = "obs", estimate = "pred")))
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
           truth = NULL,
           estimate = NULL,
           dnn = c("Prediction", "Truth"),
           ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    xtab <- vec2table(
      truth = get_col(data, truth),
      estimate = get_col(data, estimate),
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



#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
summary.conf_mat <- function(object, prevalence = NULL, ...) {
  xtab <- object$table
  stats <-
    tibble(accuracy = accuracy(xtab),
           kappa = NA)
  
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
        precision = precision(xtab),
        recall = recall(xtab),
        F1 = f_meas(xtab)
      )
    stats <- bind_cols(stats, stats_2class)
  }
  stats
}

