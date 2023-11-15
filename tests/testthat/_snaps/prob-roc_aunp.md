# AUNP errors on binary case

    Code
      roc_aunp(two_class_example, truth, Class1)
    Condition
      Error in `roc_aunp()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

# roc_aunp() - `options` is deprecated

    Code
      out <- roc_aunp(two_class_example, truth, Class1, Class2, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunp()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

---

    Code
      out <- roc_aunp_vec(truth = two_class_example$truth, estimate = as.matrix(
        two_class_example[c("Class1", "Class2")]), options = 1)
    Condition
      Warning:
      The `options` argument of `roc_aunp_vec()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

# work with class_pred input

    Code
      roc_aunp_vec(cp_truth, estimate)
    Condition
      Error in `roc_aunp_vec()`:
      ! `truth` should not a <class_pred> object.

