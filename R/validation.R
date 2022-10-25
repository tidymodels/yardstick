validate_numeric_truth_numeric_estimate <- function(truth, estimate) {
  if (!is.numeric(truth)) {
    cls <- class(truth)[[1]]
    abort(paste0(
      "`truth` should be a numeric, not a `", cls, "`."
    ))
  }

  if (!is.numeric(estimate)) {
    cls <- class(estimate)[[1]]
    abort(paste0(
      "`estimate` should be a numeric, not a `", cls, "`."
    ))
  }

  if (is.matrix(estimate)) {
    abort(paste0(
      "`estimate` should be a numeric vector, not a numeric matrix."
    ))
  }

  if (is.matrix(truth)) {
    abort(paste0(
      "`truth` should be a numeric vector, not a numeric matrix."
    ))
  }

  n_truth <- length(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    abort(paste0(
      "Length of `truth` (", n_truth, ") ",
      "and `estimate` (", n_estimate, ") must match."
    ))
  }
}

validate_factor_truth_factor_estimate <- function(truth, estimate) {
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }
  if (!is.factor(truth)) {
    cls <- class(truth)[[1]]
    abort(paste0(
      "`truth` should be a factor, not a `", cls, "`."
    ))
  }

  if (!is.factor(estimate)) {
    cls <- class(estimate)[[1]]
    abort(paste0(
      "`estimate` should be a factor, not a `", cls, "`."
    ))
  }

  lvls_t <- levels(truth)
  lvls_e <- levels(estimate)

  if (!identical(lvls_t, lvls_e)) {
    lvls_t <- paste0(lvls_t, collapse = ", ")
    lvls_e <- paste0(lvls_e, collapse = ", ")
    abort(
      paste0(
        "`truth` and `estimate` levels must be equivalent.\n",
        "`truth`: ",    lvls_t, "\n",
        "`estimate`: ", lvls_e, "\n"
      )
    )
  }

  n_truth <- length(truth)
  n_estimate <- length(estimate)

  if (n_truth != n_estimate) {
    abort(paste0(
      "Length of `truth` (", n_truth, ") ",
      "and `estimate` (", n_estimate, ") must match."
    ))
  }
}

validate_factor_truth_metrix_estimate <- function(truth, estimate, estimator) {
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }

  if (!is.factor(truth)) {
    cls <- class(truth)[[1]]
    abort(paste0(
      "`truth` should be a factor, not a `", cls, "`."
    ))
  }

  if (isTRUE(estimator == "binary")) {
    if (is.matrix(estimate)) {
      abort(paste0(
        "You are using a `binary` metric but have passed multiple columns to `...`"
      ))
    }

    n_lvls <- length(levels(truth))
    if (n_lvls != 2) {
      abort(paste0(
        "`estimator` is binary, only two class `truth` factors are allowed. ",
        "A factor with ", n_lvls, " levels was provided."
      ))
    }
  } else {
    n_lvls <- length(levels(truth))
    n_cols <- ncol(estimate)

    if (is.null(n_cols) || n_lvls != n_cols) {
      if (is.null(n_cols)) {
        n_cols <- 1
      }
      abort(paste0(
        "The number of levels in `truth` (", n_lvls, ") ",
        "must match the number of columns supplied in `...` (", n_cols, ")."
      ))
    }
  }
}

validate_binary_estimator <- function(truth, estimator) {
  if (estimator != "binary") return()

  lvls <- levels(truth)
  if (length(lvls) != 2) {
    abort(paste0(
      "`estimator` is binary, only two class `truth` factors are allowed. ",
      "A factor with ", length(lvls), " levels was provided."
    ))
  }
}

# Checking column types and number supplied ------------------------------------

validate_truth_estimate_types <- function(truth, estimate, estimator) {
  UseMethod("validate_truth_estimate_types")
}

validate_truth_estimate_types.default <- function(truth, estimate, estimator) {
  cls <- class(truth)[[1]]
  abort(paste0(
    "`truth` class `", cls, "` is unknown. ",
    "`truth` must be a numeric or a factor."
  ))
}

# factor / ?
validate_truth_estimate_types.factor <- function(truth, estimate, estimator) {
  switch (estimator,
          "binary" = binary_checks(truth, estimate),
          # otherwise multiclass checks
          multiclass_checks(truth, estimate)
  )
}

# numeric / numeric
validate_truth_estimate_types.numeric <- function(truth, estimate, estimator) {

  if (!is.numeric(estimate)) {
    cls <- class(estimate)[[1]]
    abort(paste0(
      "`estimate` should be a numeric, not a `", cls, "`."
    ))
  }

  if (is.matrix(estimate)) {
    abort(paste0(
      "`estimate` should be a numeric vector, not a numeric matrix."
    ))
  }

  if (is.matrix(truth)) {
    abort(paste0(
      "`truth` should be a numeric vector, not a numeric matrix."
    ))
  }
}


# double dispatch
# truth = factor
# estimate = ?
binary_checks <- function(truth, estimate) {
  UseMethod("binary_checks", estimate)
}

# factor / unknown
binary_checks.default <- function(truth, estimate) {
  cls <- class(estimate)[[1]]
  abort(paste0(
    "A binary metric was chosen but",
    "`estimate` class `", cls, "` is unknown."
  ))
}

# factor / factor
binary_checks.factor <- function(truth, estimate) {
  lvls_t <- levels(truth)
  lvls_e <- levels(estimate)

  if (!identical(lvls_t, lvls_e)) {
    lvls_t <- paste0(lvls_t, collapse = ", ")
    lvls_e <- paste0(lvls_e, collapse = ", ")
    abort(
      paste0(
        "`truth` and `estimate` levels must be equivalent.\n",
        "`truth`: ",    lvls_t, "\n",
        "`estimate`: ", lvls_e, "\n"
      )
    )
  }

  lvls <- levels(truth)
  if (length(lvls) != 2) {
    abort(paste0(
      "`estimator` is binary, only two class `truth` factors are allowed. ",
      "A factor with ", length(lvls), " levels was provided."
    ))
  }

}

# factor / numeric
binary_checks.numeric <- function(truth, estimate) {
  # nothing to check here, all good
}

# factor / matrix
binary_checks.matrix <- function(truth, estimate) {
  abort(paste0(
    "You are using a `binary` metric but have passed multiple columns to `...`"
  ))
}

# truth = factor
# estimate = ?
multiclass_checks <- function(truth, estimate) {
  UseMethod("multiclass_checks", estimate)
}

# factor / unknown
multiclass_checks.default <- function(truth, estimate) {
  cls <- class(estimate)[[1]]
  abort(paste0("`estimate` class `", cls, "` is unknown."))
}

# factor / factor, >2 classes each
multiclass_checks.factor <- function(truth, estimate) {
  lvls_t <- levels(truth)
  lvls_e <- levels(estimate)

  if (!identical(lvls_t, lvls_e)) {
    lvls_t <- paste0(lvls_t, collapse = ", ")
    lvls_e <- paste0(lvls_e, collapse = ", ")
    abort(
      paste0(
        "`truth` and `estimate` levels must be equivalent.\n",
        "`truth`: ",    lvls_t, "\n",
        "`estimate`: ", lvls_e, "\n"
      )
    )
  }
}

# factor / numeric, but should be matrix
# (any probs function, if user went from binary->macro, they need to supply
# all cols)
multiclass_checks.numeric <- function(truth, estimate) {
  # this is bad, but we want to be consistent in erorr messages
  # with the factor / matrix check below
  multiclass_checks.matrix(truth, as.matrix(estimate))
}

# factor / matrix (any probs functions)
multiclass_checks.matrix <- function(truth, estimate) {
  n_lvls <- length(levels(truth))
  n_cols <- ncol(estimate)

  if (n_lvls != n_cols) {
    abort(paste0(
      "The number of levels in `truth` (", n_lvls, ") ",
      "must match the number of columns supplied in `...` (", n_cols, ")."
    ))
  }
}

# Check lengths of truth / estimate --------------------------------------------

validate_truth_estimate_lengths <- function(truth, estimate) {

  n_truth <- length(truth)

  if (is.matrix(estimate)) {
    n_estimate <- nrow(estimate)
  }
  else {
    n_estimate <- length(estimate)
  }

  if (n_truth != n_estimate) {
    abort(paste0(
      "Length of `truth` (", n_truth, ") ",
      "and `estimate` (", n_estimate, ") must match."
    ))
  }
}

# Validate the initial class of the inputs -------------------------------------

validate_class <- function(x, nm, cls) {

  # cls is always known to have a `is.cls()` function
  is_cls <- get(paste0("is.", cls))

  if(!is_cls(x)) {
    cls_real <- class(x)[[1]]
    abort(paste0(
      "`", nm, "` ",
      "should be a ", cls, " ",
      "but a ", cls_real, " was supplied."
    ))
  }
}

validate_truth_estimate_checks <- function(truth, estimate,
                                           cls = "numeric",
                                           estimator) {

  if(length(cls) == 1) {
    cls <- c(cls, cls)
  }

  # validate_class(truth, "truth", cls[1])
  # validate_class(estimate, "estimate", cls[2])
  # validate_truth_estimate_types(truth, estimate, estimator)
  # validate_truth_estimate_lengths(truth, estimate)
}

# Validate estimator type is allowed -------------------------------------------

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
validate_estimator <- function(estimator, estimator_override = NULL) {

  if(is.null(estimator)) {
    return()
  }

  if (!is.null(estimator_override)) {
    allowed <- estimator_override
  }
  else {
    allowed <- c("binary", "macro", "micro", "macro_weighted")
  }

  if (length(estimator) != 1) {
    abort(paste0(
      "`estimator` must be length 1, not ", length(estimator), "."
    ))
  }

  if (!is.character(estimator)) {
    abort(paste0(
      "`estimator` must be a character, not a ", class(estimator)[1], "."
    ))
  }

  estimator_ok <- (estimator %in% allowed)

  if (!estimator_ok) {
    allowed <- paste0(dQuote(allowed), collapse = ", ")
    abort(paste0(
      "`estimator` must be one of: ", allowed,
      ". Not ", dQuote(estimator), "."
    ))
  }

}

validate_case_weights <- function(case_weights, size) {
  if (is.null(case_weights)) {
    return(invisible())
  }

  if (!is.integer(case_weights) && !is.double(case_weights)) {
    abort("`case_weights` must be an integer or double vector.")
  }

  size_case_weights <- length(case_weights)

  if (size_case_weights != size) {
    abort(paste0(
      "`case_weights` (", size_case_weights, ") must have the same ",
      "length as `truth` (", size, ")."
    ))
  }

  invisible()
}
