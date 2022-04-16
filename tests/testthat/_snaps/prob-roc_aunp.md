# AUNP errors on binary case

    Code
      (expect_error(roc_aunp(two_class_example, truth, Class1)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = metric_fn(truth = truth, estimate = Class1, na_rm = na_rm)`.
      Caused by error in `multiclass_checks.matrix()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

# roc_aunp() - `options` is deprecated

    Code
      out <- roc_aunp(two_class_example, truth, Class1, Class2, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunp()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

---

    Code
      out <- roc_aunp_vec(truth = two_class_example$truth, estimate = as.matrix(
        two_class_example[c("Class1", "Class2")]), options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunp_vec()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

