# These should be refactored and made simpler

#' @importFrom rlang with_handlers enquo quos abort
#' @importFrom tidyselect vars_select vars_pull

all_select <- function(data, truth, estimate, ...) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  validate_not_missing(truth, "truth")
  validate_not_missing(estimate, "estimate")

  truth_var <- tidyselect::vars_pull(names(data), !! truth)
  est_var <- tidyselect::vars_pull(names(data), !! estimate)

  dot_vars <- tidyselect::vars_select(names(data), !!! quos(...))

  if (length(dot_vars) == 0) {
    dot_vars <- NA
  } else {
    prob_num <- vapply(data[, dot_vars, drop = FALSE],
                       is.numeric, logical(1))
    if(any(!prob_num))
      stop("The columns selected for class probabilities should ",
           "be numeric: ",
           paste0("`", names(prob_num)[!prob_num], "`", collapse = ", "),
           call. = FALSE)
  }

  list(truth = truth_var, estimate = est_var, probs = unname(dot_vars))
}

