#' @importFrom dplyr summarise
metric_summarizer <- function(metric_nm, metric_fn, data, truth, estimate,
                              averaging = NA, na.rm = TRUE, ...,
                              metric_fn_options = list()) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  validate_not_missing(truth, "truth")
  validate_not_missing(estimate, "estimate")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)
  estimate <- handle_chr_names(estimate)

  metric_tbl <- summarise(
    data,
    .metric = construct_name(!! metric_nm, averaging, !! estimate),
    .estimate = metric_fn(
      truth = !! truth,
      estimate = !! estimate,
      !!! spliceable_averaging(averaging),
      na.rm = na.rm,
      !!! metric_fn_options
    )
  )

  dplyr::as_tibble(metric_tbl)
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
                                na.rm = TRUE,
                                cls = "numeric",
                                averaging = NULL,
                                averaging_override = NULL,
                                ...) {

  validate_averaging(averaging, averaging_override)
  validate_truth_estimate_checks(truth, estimate, cls, averaging)

  if (na.rm) {
    complete_cases <- complete.cases(truth, estimate)
    truth <- truth[complete_cases]

    if (is.matrix(estimate)) {
      estimate <- estimate[complete_cases, , drop = FALSE]
    }
    else {
      estimate <- estimate[complete_cases]
    }

  }
  # na.rm = FALSE
  # return NA if any NA
  else {

    if (anyNA(truth) || anyNA(estimate)) {
      return(NA_real_)
    }

  }

  metric_impl(truth, estimate, ...)
}

metric_tibbler <- function(.metric, .estimate) {
  dplyr::tibble(.metric = .metric, .estimate = .estimate)
}

# if averaging = NULL, we don't want to pass it along
# as an argument. splicing in an empty list essentially is equivalent
# to splicing in nothing
# averaging = NA -> not applicable for this metric
# averaging = NULL -> should be chosen automatically for this metric
spliceable_averaging <- function(averaging) {

  if(is.null(averaging) || !is.na(averaging)) {
    return(list(averaging = averaging))
  }
  else{
    return(list())
  }

}
