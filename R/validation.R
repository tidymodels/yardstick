validate_numeric_truth_numeric_estimate <- function(
  truth,
  estimate,
  call = caller_env()
) {
  if (!is.numeric(truth)) {
    cli::cli_abort(
      "{.arg truth} should be a numeric vector,
      not {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (!is.numeric(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a numeric vector,
      not {.obj_type_friendly {estimate}}.",
      call = call
    )
  }

  if (is.matrix(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a numeric vector, not a numeric matrix.",
      call = call
    )
  }

  if (is.matrix(truth)) {
    cli::cli_abort(
      "{.arg truth} should be a numeric vector, not a numeric matrix.",
      call = call
    )
  }

  n_truth <- length(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    cli::cli_abort(
      "{.arg truth} ({n_truth}) and
      {.arg estimate} ({n_estimate}) must be the same length.",
      call = call
    )
  }
}

validate_factor_truth_factor_estimate <- function(
  truth,
  estimate,
  call = caller_env()
) {
  if (!is.factor(truth)) {
    cli::cli_abort(
      "{.arg truth} should be a factor,
      not a {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (!is.factor(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a factor,
      not a {.obj_type_friendly {estimate}}.",
      call = call
    )
  }

  lvls_t <- levels(truth)
  lvls_e <- levels(estimate)

  if (!identical(lvls_t, lvls_e)) {
    cli::cli_abort(
      c(
        "x" = "{.arg truth} and {.arg estimate} levels must be equivalent.",
        "*" = "{.arg truth}: {lvls_t}.",
        "*" = "{.arg estimate}: {lvls_e}."
      ),
      call = call
    )
  }

  n_truth <- length(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    cli::cli_abort(
      "{.arg truth} ({n_truth}) and
      {.arg estimate} ({n_estimate}) must be the same length.",
      call = call
    )
  }
}

validate_factor_truth_matrix_estimate <- function(
  truth,
  estimate,
  estimator,
  call = caller_env()
) {
  if (!is.factor(truth)) {
    cli::cli_abort(
      "{.arg truth} should be a factor,
      not a {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (estimator == "binary") {
    if (is.matrix(estimate)) {
      cli::cli_abort(
        "You are using a binary metric but have passed multiple columns to
        {.arg ...}.",
        call = call
      )
    }

    if (!is.numeric(estimate)) {
      cli::cli_abort(
        "{.arg estimate} should be a numeric vector,
        not {.obj_type_friendly {estimate}}.",
        call = call
      )
    }

    n_lvls <- length(levels(truth))
    if (n_lvls != 2) {
      cli::cli_abort(
        "{.arg estimator} is binary, only two class {.arg truth} factors are
        allowed. A factor with {n_lvls} levels was provided.",
        call = call
      )
    }
  } else {
    n_lvls <- length(levels(truth))
    if (is.matrix(estimate)) {
      n_cols <- ncol(estimate)
    } else {
      n_cols <- 1L
    }

    if (n_lvls != n_cols) {
      cli::cli_abort(
        "The number of levels in `truth` ({n_lvls})
        must match the number of columns supplied in `...` ({n_cols}).",
        call = call
      )
    }

    if (!is.numeric(as.vector(estimate))) {
      cls <- as.vector(estimate)
      cli::cli_abort(
        "The columns supplied in {.arg ...} should be numerics,
        not {.cls cls}.",
        call = call
      )
    }
  }
}

validate_ordered_truth_matrix_estimate <- function(
  truth,
  estimate,
  estimator,
  call = caller_env()
) {
  if (!is.ordered(truth)) {
    cli::cli_abort(
      "{.arg truth} should be a ordered factor,
      not a {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (estimator == "binary") {
    if (is.matrix(estimate)) {
      cli::cli_abort(
        "You are using a binary metric but have passed multiple columns to
        {.arg ...}.",
        call = call
      )
    }

    if (!is.numeric(estimate)) {
      cli::cli_abort(
        "{.arg estimate} should be a numeric vector,
        not {.obj_type_friendly {estimate}}.",
        call = call
      )
    }

    n_lvls <- length(levels(truth))
    if (n_lvls != 2) {
      cli::cli_abort(
        "{.arg estimator} is binary, only two class {.arg truth} factors are
        allowed. A factor with {n_lvls} levels was provided.",
        call = call
      )
    }
  } else {
    n_lvls <- length(levels(truth))
    if (is.matrix(estimate)) {
      n_cols <- ncol(estimate)
    } else {
      n_cols <- 1L
    }

    if (n_lvls != n_cols) {
      cli::cli_abort(
        "The number of levels in `truth` ({n_lvls})
        must match the number of columns supplied in `...` ({n_cols}).",
        call = call
      )
    }

    if (!is.numeric(as.vector(estimate))) {
      cls <- as.vector(estimate)
      cli::cli_abort(
        "The columns supplied in {.arg ...} should be numerics,
        not {.cls cls}.",
        call = call
      )
    }
  }
}

validate_surv_truth_list_estimate <- function(
  truth,
  estimate,
  call = caller_env()
) {
  if (!inherits(truth, "Surv")) {
    cli::cli_abort(
      "`truth` should be a Surv object,
      not a {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (!is.list(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a list,
      not a {.obj_type_friendly {estimate}}.",
      call = call
    )
  }

  if (!all(vapply(estimate, is.data.frame, FUN.VALUE = logical(1)))) {
    cli::cli_abort(
      "All elements of {.arg estimate} should be data.frames.",
      call = call
    )
  }

  valid_names <- c(".eval_time", ".pred_survival", ".weight_censored")
  has_names <- vapply(
    estimate,
    function(x) all(valid_names %in% names(x)),
    FUN.VALUE = logical(1)
  )

  if (!all(has_names)) {
    cli::cli_abort(
      "All data.frames of {.arg estimate} should include column names:
      {.field (.eval_time)}, {.field (.pred_survival)}, and
      {.field (.weight_censored)}.",
      call = call
    )
  }

  n_truth <- nrow(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    cli::cli_abort(
      "{.arg truth} ({n_truth}) and
      {.arg estimate} ({n_estimate}) must be the same length.",
      call = call
    )
  }

  eval_time_cols <- lapply(estimate, function(x) x$.eval_time)

  if (length(unique(eval_time_cols)) > 1) {
    offenders <- vapply(
      eval_time_cols,
      function(x) !identical(x, eval_time_cols[[1]]),
      logical(1)
    )
    offenders <- which(offenders)

    cli::cli_abort(
      c(
        x = "All the {.field .eval_time} columns of {.arg estimate} must be
            identical.",
        i = "The folllowing index differed from the first: {.val {offenders}}."
      ),
      call = call
    )
  }

  eval_time <- eval_time_cols[[1]]

  if (any(is.na(eval_time))) {
    cli::cli_abort(
      c(
        x = "Missing values in {.field .eval_time} are not allowed."
      ),
      call = call
    )
  }

  if (any(eval_time < 0)) {
    offenders <- unique(eval_time[eval_time < 0])

    cli::cli_abort(
      c(
        x = "Negative values of {.field .eval_time} are not allowed.",
        i = "The following negative values were found: {.val {offenders}}."
      ),
      call = call
    )
  }

  if (any(is.infinite(eval_time))) {
    cli::cli_abort(
      c(
        x = "Infinite values of {.field .eval_time} are not allowed."
      ),
      call = call
    )
  }

  if (any(duplicated(eval_time))) {
    cli::cli_abort(
      c(
        x = "Duplicate values of {.field .eval_time} are not allowed."
      ),
      call = call
    )
  }
}

validate_surv_truth_numeric_estimate <- function(
  truth,
  estimate,
  call = caller_env()
) {
  if (!.is_surv(truth, fail = FALSE)) {
    cli::cli_abort(
      "{.arg truth} should be a Surv object,
      not {.obj_type_friendly {truth}}.",
      call = call
    )
  }

  if (!is.numeric(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a numeric vector,
      not {.obj_type_friendly {estimate}}.",
      call = call
    )
  }

  if (is.matrix(estimate)) {
    cli::cli_abort(
      "{.arg estimate} should be a numeric vector, not a numeric matrix.",
      call = call
    )
  }

  n_truth <- nrow(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    cli::cli_abort(
      "{.arg truth} ({n_truth}) and
      {.arg estimate} ({n_estimate}) must be the same length.",
      call = call
    )
  }
}

validate_binary_estimator <- function(truth, estimator, call = caller_env()) {
  if (estimator != "binary") {
    return()
  }

  lvls <- levels(truth)
  if (length(lvls) != 2) {
    cli::cli_abort(
      "{.arg estimator} is binary, only two class {.arg truth} factors are
      allowed. A factor with {length(lvls)} levels was provided.",
      call = call
    )
  }
}

#' @section Estimator Validation:
#' `validate_estimator()` is called from your metric specific method of
#' `finalize_estimator_internal()` and ensures that a user provided estimator
#' is of the right format and is one of the allowed values.
#'
#' @param estimator_override A character vector overriding the default allowed
#' estimator list of
#' `c("binary", "macro", "micro", "macro_weighted")`. Set
#' this if your classification estimator does not support all of these methods.
#' @rdname developer-helpers
#' @export
validate_estimator <- function(
  estimator,
  estimator_override = NULL,
  call = caller_env()
) {
  if (is.null(estimator)) {
    return()
  }

  if (!is.null(estimator_override)) {
    allowed <- estimator_override
  } else {
    allowed <- c("binary", "macro", "micro", "macro_weighted")
  }

  estimator <- rlang::arg_match(estimator, allowed, error_call = call)
  invisible(NULL)
}

validate_case_weights <- function(case_weights, size, call = caller_env()) {
  if (is.null(case_weights)) {
    return(invisible())
  }

  size_case_weights <- length(case_weights)

  if (size_case_weights != size) {
    cli::cli_abort(
      "{.arg truth} ({size}) and
      {.arg case_weights} ({size_case_weights}) must be the same length.",
      call = call
    )
  }

  invisible(NULL)
}
