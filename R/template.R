#' @importFrom tibble as_tibble
#' @importFrom dplyr summarise
metric_summarizer <- function(metric_nm, metric_fn, data, truth, estimate,
                              na.rm = TRUE, ..., metric_fn_options = list()) {

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(enquo(truth))
  estimate <- handle_chr_names(enquo(estimate))

  metric_tbl <- summarise(
    data,
    .metric = !! metric_nm,
    .estimate = metric_fn(
      truth = !! truth,
      estimate = !! estimate,
      na.rm = na.rm,
      !!! metric_fn_options
    )
  )

  as_tibble(metric_tbl)
}

#' @importFrom rlang get_expr set_expr
handle_chr_names <- function(x) {
  x_expr <- get_expr(x)

  # Replace character with bare name
  if(is.character(x_expr) && length(x_expr) == 1) {
    x <- set_expr(x, as.name(x_expr))
  }

  x
}

#' @importFrom stats complete.cases
metric_vec_template <- function(metric_impl, truth, estimate,
                                na.rm = TRUE, cls = "numeric", ...) {

  validate_truth_estimate_checks(truth, estimate, cls)

  if (na.rm) {
    complete_cases <- complete.cases(truth, estimate)
    truth <- truth[complete_cases]

    if(NCOL(estimate) == 1) {
      estimate <- estimate[complete_cases]
    } else {
      # estimate can be a matrix
      estimate <- estimate[complete_cases, , drop = FALSE]
    }

  }

  metric_impl(truth, estimate, ...)
}

metric_tibbler <- function(.metric, .estimate) {
  tibble::tibble(.metric = .metric, .estimate = .estimate)
}

validate_truth_estimate_lengths <- function(truth, estimate) {

  # use NROW not length so an estimate matrix and truth vec can be compared
  n_truth <- NROW(truth)
  n_estimate <- NROW(estimate)

  if(n_truth != n_estimate) {
    msg <- paste0("Length of `truth` (", n_truth,
                  ") and `estimate` (", n_estimate, ") must match.")
    stop(msg, call. = FALSE)
  }
}

validate_class <- function(x, nm, cls) {
  if(!inherits(x, cls)) {
    stop("`", nm, "` should be a ", cls, call. = FALSE)
  }
}

validate_truth_estimate_checks <- function(truth, estimate, cls = "numeric") {

  truth_cls <- cls[1]

  # Allow cls to be a vector of length 2
  if(length(cls) > 1) {
    estimate_cls <- cls[2]
  } else {
    estimate_cls <- truth_cls
  }

  validate_truth_estimate_lengths(truth, estimate)
  validate_class(truth, "truth", truth_cls)
  validate_class(estimate, "estimate", estimate_cls)
}
