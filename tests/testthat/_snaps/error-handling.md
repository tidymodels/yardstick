# bad args

    Code
      sens(pathology, truth = "patholosgy", estimate = "scan")
    Condition
      Error in `sens()`:
      ! Can't select columns that don't exist.
      x Column `patholosgy` doesn't exist.

# `truth` should be factor

    Code
      sens(df, truth, estimate)
    Condition
      Error in `sens()`:
      ! `truth` should be a factor, not a a number.

# At least 2 levels in truth

    Code
      sens(df, truth, estimate)
    Condition
      Error in `sens()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 1 levels was provided.

# Single character values are caught with correct errors

    Code
      sens(pathology, "a", scan)
    Condition
      Error in `sens()`:
      ! Can't select columns that don't exist.
      x Column `a` doesn't exist.

# Bad unquoted input is caught

    Code
      sens(pathology, !!bad, scan)
    Condition
      Error in `sens()`:
      ! Can't select columns that don't exist.
      x Column `a` doesn't exist.

# Non-allowed estimator

    Code
      sens(pathology, pathology, scan, estimator = "blah")
    Condition
      Error in `sens()`:
      ! `estimator` must be one of "binary", "macro", "micro", or "macro_weighted", not "blah".

# Bad estimator + truth combination

    Code
      sens(hpc_cv, obs, pred, estimator = "binary")
    Condition
      Error in `sens()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 4 levels was provided.

# Bad estimator type

    Code
      sens(hpc_cv, obs, pred, estimator = 1)
    Condition
      Error in `sens()`:
      ! `estimator` must be a character vector, not the number 1.

---

    Code
      sens(hpc_cv, obs, pred, estimator = c("1", "2"))
    Condition
      Error in `sens()`:
      ! `estimator` must be one of "binary", "macro", "micro", or "macro_weighted", not "1".

# Numeric matrix in numeric metric

    Code
      rmse(solubility_test, a, prediction)
    Condition
      Error in `rmse()`:
      ! `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      rmse(solubility_test, solubility, a)
    Condition
      Error in `rmse()`:
      ! `estimate` should be a numeric vector, not a numeric matrix.

# Factors with non identical levels

    Code
      sens(df, x, y)
    Condition
      Error in `sens()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: a, b, and c.
      * `estimate`: a and b.

# Multiple estimate columns for a binary metric

    Code
      roc_auc(two_class_example, truth, Class1:Class2)
    Condition
      Error in `roc_auc()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

# 1 estimate column for a multiclass metric

    Code
      roc_auc(hpc_cv, obs, VF)
    Condition
      Error in `roc_auc()`:
      ! The number of levels in `truth` (4) must match the number of columns supplied in `...` (1).

# `truth` and `estimate` of different lengths

    Code
      rmse_vec(1:5, 1:6)
    Condition
      Error in `rmse_vec()`:
      ! `truth` (5) and `estimate` (6) must be the same length.

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
      Error in `sens()`:
      ! `x` must have equal dimensions. `x` has 3 columns and 2 rows.

---

    Code
      sens(as.table(matrix(1:4, 2, dimnames = list(c("A", "B"), c("A", "C")))))
    Condition
      Error in `sens()`:
      ! The table must the same groups in the same order.

