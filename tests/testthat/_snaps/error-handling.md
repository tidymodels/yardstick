# `truth` should be factor

    Code
      (expect_error(sens(df, truth, estimate)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` should be a factor, not a `numeric`.

# At least 2 levels in truth

    Code
      (expect_error(sens(df, truth, estimate)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 1 levels was provided.

# Single character values are caught with correct errors

    Code
      (expect_error(sens(pathology, "a", scan)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `sens()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# Bad unquoted input is caught

    Code
      (expect_error(sens(pathology, !!bad, scan)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `sens()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# Non-allowed estimator

    Code
      (expect_error(sens(pathology, pathology, scan, estimator = "blah")))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(.data[["pathology"]], estimator, name)`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be one of: "binary", "macro", "micro", "macro_weighted". Not "blah".

# Bad estimator + truth combination

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = "binary")))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 4 levels was provided.

# Bad estimator type

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = 1)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(.data[["obs"]], estimator, name)`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be a character, not a numeric.

---

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = c("1", "2"))))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimator = finalize_estimator(.data[["obs"]], estimator, name)`.
      Caused by error in `validate_estimator()`:
      ! `estimator` must be length 1, not 2.

# Numeric matrix in numeric metric

    Code
      (expect_error(rmse(solubility_test, a, prediction)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_numeric_truth_numeric_estimate()`:
      ! `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      (expect_error(rmse(solubility_test, solubility, a)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric vector, not a numeric matrix.

# Factors with non identical levels

    Code
      (expect_error(sens(df, x, y)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: a, b, c
      `estimate`: a, b

# Multiple estimate columns for a binary metric

    Code
      (expect_error(roc_auc(two_class_example, truth, Class1:Class2)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_matrix_estimate()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

# 1 estimate column for a multiclass metric

    Code
      (expect_error(roc_auc(hpc_cv, obs, VF)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_matrix_estimate()`:
      ! The number of levels in `truth` (4) must match the number of columns supplied in `...` (1).

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

