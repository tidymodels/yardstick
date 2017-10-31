#' Confusion Matrix for Categorical Data
#'
#' Calculates a cross-tabulation of observed and predicted classes with
#' associated statistics.
#'
#' The functions requires that the factors have exactly the same levels.
#'
#' @aliases conf_mat.table conf_mat.default conf_mat
#' @param data A data frame or a [base::table()].
#' @param truth A string corresponding to the column in the data frame containig  the factor of classes to be used as the true results
#' @param estimate A string corresponding to the column in the data frame containig  the factor of predicted classes.
#' @param dnn a character vector of dimnames for the table
#' @param ... Options to pass to [base::table()] (not including `dnn`)
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
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
summary.conf_mat <- function(object, prevalence = NULL, ...) {
  xtab <- object$table
  stats <-
    tibble(accuracy = sum(diag(xtab)) / sum(xtab),
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
        F1 = F_meas(xtab)
      )
    stats <- bind_cols(stats, stats_2class)
  }
  stats
}
