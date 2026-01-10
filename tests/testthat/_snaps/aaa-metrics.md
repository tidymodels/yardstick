# bad args

    Code
      metrics(two_class_example, truth, Class1)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `accuracy()`.
      Caused by error:
      ! `estimate` should be a factor, not a a double vector.

---

    Code
      metrics(two_class_example, Class1, truth)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `rmse()`.
      Caused by error:
      ! `estimate` should be a numeric vector, not a <factor> object.

---

    Code
      metrics(hpc_cv, "obs", "pred", VF:M)
    Condition
      Error in `mn_log_loss()`:
      ! The number of levels in `truth` (4) must match the number of columns supplied in `...` (3).

# metrics() - `options` is deprecated

    Code
      out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `metrics()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

