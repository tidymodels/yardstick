# errors with class_pred input

    Code
      roc_aunu_vec(cp_truth, estimate)
    Condition
      Error in `roc_aunu_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      roc_aunu_vec(1, 1, na_rm = "yes")
    Condition
      Error in `roc_aunu_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# `options` is deprecated

    Code
      out <- roc_aunu(two_class_example, truth, Class1, Class2, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunu()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

---

    Code
      out <- roc_aunu_vec(truth = two_class_example$truth, estimate = as.matrix(
        two_class_example[c("Class1", "Class2")]), options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunu_vec()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

# errors on binary case

    Code
      roc_aunu(two_class_example, truth, Class1)
    Condition
      Error in `roc_aunu()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

