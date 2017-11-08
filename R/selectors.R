# These should be refactored and made simpler

#' @importFrom rlang exiting abort
abort_selection <- rlang::exiting(function(cnd) {
  rlang::abort("No variables or terms were selected.")
})

#' @importFrom rlang with_handlers enquo quos
#' @importFrom tidyselect vars_select vars_pull

prob_select <- function(data, truth, ...) {
  truth_var <- tidyselect::vars_pull(names(data), !! enquo(truth))
  dot_vars <- rlang::with_handlers(
    tidyselect::vars_select(names(data), !!! quos(...)),
    tidyselect_empty = abort_selection
  )
  if (length(dot_vars) == 0) {
    stop("No class probability columns were selected by the `...`.",
         call. = FALSE)
  }
  if(!is.factor(data[[truth_var]]))
    stop("`", truth_var, "` should be a factor.", call. = FALSE)
  prob_num <- vapply(data[, dot_vars, drop = FALSE], 
                     is.numeric, logical(1))
  if(any(!prob_num))
    stop("The columns selected for class probabilities should ",
         "be numeric: ", 
         paste0("`", names(prob_num)[!prob_num], "`", collapse = ", "),
         call. = FALSE)
  
  list(truth = truth_var, probs = unname(dot_vars))
}

all_select <- function(data, truth, estimate, ...) {
  truth_var <- tidyselect::vars_pull(names(data), !! enquo(truth))
  est_var <- tidyselect::vars_pull(names(data), !! enquo(estimate))
  
  dot_vars <- rlang::with_handlers(
    tidyselect::vars_select(names(data), !!! quos(...)),
    tidyselect_empty = abort_selection
  )
  
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


factor_select <- function(data, truth, estimate, ...) {
  truth_var <- tidyselect::vars_pull(names(data), !! enquo(truth))
  est_var <- tidyselect::vars_pull(names(data), !! enquo(estimate))
  if(!is.factor(data[[truth_var]]))
    stop("`", truth_var, "` should be a factor.", call. = FALSE)
  if(!is.factor(data[[est_var]]))
    stop("`", est_var, "` should be a factor.", call. = FALSE)
  
  list(truth = truth_var, estimate = est_var)
}


num_select <- function(data, truth, estimate, ...) {
  truth_var <- tidyselect::vars_pull(names(data), !! enquo(truth))
  est_var <- tidyselect::vars_pull(names(data), !! enquo(estimate))
  if(!is.numeric(data[[truth_var]]))
    stop("`", truth_var, "` should be numeric", call. = FALSE)
  if(!is.numeric(data[[est_var]]))
    stop("`", est_var, "` should be numeric.", call. = FALSE)
  
  list(truth = truth_var, estimate = est_var)
}

