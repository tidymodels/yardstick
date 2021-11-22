# `truth` should be factor

    Code
      (expect_error(sens(pathology, 1, factor("A"))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = 1, estimate = factor("A"), na_rm = na_rm, event_level = "first")`.
      x `truth` should be a factor but a numeric was supplied.

# At least 2 levels in truth

    Code
      (expect_error(sens(pathology, factor("A"), factor("A"))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `estimator` is binary, only two class `truth` factors are allowed. A factor with 1 levels was provided.

# Single character values are caught with correct errors

    Code
      (expect_error(sens(pathology, "a", factor("A"))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `truth` should be a factor but a character was supplied.

# Bad unquoted input is caught

    Code
      (expect_error(sens(pathology, c("a", "b"), factor("A"))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `truth` should be a factor but a character was supplied.

# Non-allowed estimator

    Code
      (expect_error(sens(pathology, pathology, scan, estimator = "blah")))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimator`.
      i `.estimator = eval_tidy(finalize_estimator_expr)`.
      x `estimator` must be one of: "binary", "macro", "micro", "macro_weighted". Not "blah".

# Bad estimator + truth combination

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = "binary")))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `estimator` is binary, only two class `truth` factors are allowed. A factor with 4 levels was provided.

# Bad estimator type

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = 1)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimator`.
      i `.estimator = eval_tidy(finalize_estimator_expr)`.
      x `estimator` must be a character, not a numeric.

---

    Code
      (expect_error(sens(hpc_cv, obs, pred, estimator = c("1", "2"))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimator`.
      i `.estimator = eval_tidy(finalize_estimator_expr)`.
      x `estimator` must be length 1, not 2.

# Numeric matrix in numeric metric

    Code
      (expect_error(rmse(solubility_test, matrix(1:5), prediction)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = matrix(1:5), estimate = prediction, na_rm = na_rm)`.
      x `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      (expect_error(rmse(solubility_test, solubility, matrix(1:5))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = solubility, estimate = matrix(1:5), na_rm = na_rm)`.
      x `estimate` should be a numeric vector, not a numeric matrix.

# Factors with non identical levels

    Code
      (expect_error(sens(df, x, y)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = x, estimate = y, na_rm = na_rm, event_level = "first")`.
      x `truth` and `estimate` levels must be equivalent.
      `truth`: a, b, c
      `estimate`: a, b

# Multiple estimate columns for a binary metric

    Code
      (expect_error(roc_auc(two_class_example, truth, Class1:Class2)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x You are using a `binary` metric but have passed multiple columns to `...`

# 1 estimate column for a multiclass metric

    Code
      (expect_error(roc_auc(hpc_cv, obs, VF)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x The number of levels in `truth` (4) must match the number of columns supplied in `...` (1).

