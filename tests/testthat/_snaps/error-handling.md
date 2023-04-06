# bad args

    Code
      sens(pathology, truth = "patholosgy", estimate = "scan")
    Condition
      Error in `sens()`:
      ! Can't subset columns that don't exist.
      x Column `patholosgy` doesn't exist.

# `truth` should be factor

    Code
      sens(df, truth, estimate)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` should be a factor, not a `numeric`.

# At least 2 levels in truth

    Code
      sens(df, truth, estimate)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 1 levels was provided.

# Single character values are caught with correct errors

    Code
      sens(pathology, "a", scan)
    Condition
      Error in `sens()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# Bad unquoted input is caught

    Code
      sens(pathology, !!bad, scan)
    Condition
      Error in `sens()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# Non-allowed estimator

    Code
      sens(pathology, pathology, scan, estimator = "blah")
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(...)`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be one of: "binary", "macro", "micro", "macro_weighted". Not "blah".

# Bad estimator + truth combination

    Code
      sens(hpc_cv, obs, pred, estimator = "binary")
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 4 levels was provided.

# Bad estimator type

    Code
      sens(hpc_cv, obs, pred, estimator = 1)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(.data[["obs"]], .env[["estimator"]], .env[["name"]])`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be a character, not a numeric.

---

    Code
      sens(hpc_cv, obs, pred, estimator = c("1", "2"))
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(.data[["obs"]], .env[["estimator"]], .env[["name"]])`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be length 1, not 2.

# Numeric matrix in numeric metric

    Code
      rmse(solubility_test, a, prediction)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      rmse(solubility_test, solubility, a)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric vector, not a numeric matrix.

# Factors with non identical levels

    Code
      sens(df, x, y)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: a, b, c
      `estimate`: a, b

# Multiple estimate columns for a binary metric

    Code
      roc_auc(two_class_example, truth, Class1:Class2)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_matrix_estimate()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

# 1 estimate column for a multiclass metric

    Code
      roc_auc(hpc_cv, obs, VF)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_matrix_estimate()`:
      ! The number of levels in `truth` (4) must match the number of columns supplied in `...` (1).

# `truth` and `estimate` of different lengths

    Code
      rmse_vec(1:5, 1:6)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! Length of `truth` (5) and `estimate` (6) must match.

# Missing arguments

    Code
      sens(two_class_example)
    Condition
      Error in `sens()`:
      ! Must select at least one item.

---

    Code
      sens(two_class_example, truth)
    Condition
      Error in `sens()`:
      ! Must select at least one item.

# Table with bad format

    Code
      sens(as.table(matrix(1:6, 2)))
    Condition
      Error:
      ! the table must have nrow = ncol

---

    Code
      sens(as.table(matrix(1:4, 2, dimnames = list(c("A", "B"), c("A", "C")))))
    Condition
      Error:
      ! the table must the same groups in the same order

