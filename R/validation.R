# Checking column types and number supplied ------------------------------------

validate_truth_estimate_types <- function(truth, estimate, averaging) {
  UseMethod("validate_truth_estimate_types")
}

validate_truth_estimate_types.default <- function(truth, estimate, averaging) {
  cls <- class(truth)[[1]]
  abort(paste0(
    "`truth` class `", cls, "` is unknown. ",
    "`truth` must be a numeric or a factor."
  ))
}

# factor / ?
validate_truth_estimate_types.factor <- function(truth, estimate, averaging) {
  switch (averaging,
          "binary" = binary_checks(truth, estimate),
          # otherwise multiclass checks
          multiclass_checks(truth, estimate)
  )
}

# numeric / numeric
validate_truth_estimate_types.numeric <- function(truth, estimate, averaging) {
  if (!is.null(averaging)) {
    abort(paste0(
      "`averaging` was somehow applied to a numeric metric.",
      "This should never happen."
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
  binary_checks.factor(truth, estimate)
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
                                           averaging = NULL) {

  if(length(cls) == 1) {
    cls <- c(cls, cls)
  }

  validate_class(truth, "truth", cls[1])
  validate_class(estimate, "estimate", cls[2])
  validate_truth_estimate_types(truth, estimate, averaging)
  validate_truth_estimate_lengths(truth, estimate)
}

# Validate that the user supplied an input

validate_not_missing <- function(x, nm) {
  if(rlang::quo_is_missing(x)) {
   abort(paste0(
     "`", nm, "` ",
     "is missing and must be supplied."
   ))
  }
}
