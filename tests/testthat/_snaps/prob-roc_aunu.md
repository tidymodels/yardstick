# AUNU errors on binary case

    Code
      (expect_error(roc_aunu(two_class_example, truth, Class1)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = fn(...)`.
      Caused by error in `multiclass_checks.matrix()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

# roc_aunu() - `options` is deprecated

    Code
      out <- roc_aunu(two_class_example, truth, Class1, Class2, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunu()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

---

    Code
      out <- roc_aunu_vec(truth = two_class_example$truth, estimate = as.matrix(
        two_class_example[c("Class1", "Class2")]), options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunu_vec()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

