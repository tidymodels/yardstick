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
      metrics(three_class, "obs", "pred", setosa, versicolor)
    Condition
      Error in `mn_log_loss()`:
      ! The number of levels in `truth` (3) must match the number of columns supplied in `...` (2).

# metrics() - `options` is deprecated

    Code
      out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `metrics()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

# numeric metric sets

    Code
      metric_set(rmse, "x")
    Condition
      Error in `metric_set()`:
      ! All inputs to `metric_set()` must be functions. These inputs are not: 2.

# mixing bad metric sets

    Code
      metric_set(rmse, accuracy)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - numeric (rmse)
      - class (accuracy)

# print metric_set works

    Code
      metric_set(rmse, rsq, ccc)
    Output
      A metric set, consisting of:
      - `rmse()`, a numeric metric | direction: minimize
      - `rsq()`, a numeric metric  | direction: maximize
      - `ccc()`, a numeric metric  | direction: maximize

# `metric_set()` errors contain env name for unknown functions (#128)

    Code
      metric_set(accuracy, foobar, sens, rlang::abort)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - class (accuracy, sens)
      - other (foobar <test>, abort <namespace:rlang>)

---

    Code
      metric_set(accuracy, foobar, sens, rlang::abort)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - class (accuracy, sens)
      - other (foobar <test>, abort <namespace:rlang>)

# `metric_set()` gives an informative error for a single non-metric function (#181)

    Code
      metric_set(foobar)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - other (foobar <test>)

# errors informatively for unevaluated metric factories

    Code
      metric_set(demographic_parity)
    Condition
      Error in `metric_set()`:
      ! The input `demographic_parity` is a groupwise metric (`?yardstick::new_groupwise_metric()`) factory and must be passed a data-column before addition to a metric set.
      i Did you mean to type e.g. `demographic_parity(col_name)`?

---

    Code
      metric_set(demographic_parity, roc_auc)
    Condition
      Error in `metric_set()`:
      ! The input `demographic_parity` is a groupwise metric (`?yardstick::new_groupwise_metric()`) factory and must be passed a data-column before addition to a metric set.
      i Did you mean to type e.g. `demographic_parity(col_name)`?

---

    Code
      metric_set(demographic_parity, equal_opportunity)
    Condition
      Error in `metric_set()`:
      ! The inputs `demographic_parity` and `equal_opportunity` are groupwise metric (`?yardstick::new_groupwise_metric()`) factories and must be passed a data-column before addition to a metric set.
      i Did you mean to type e.g. `demographic_parity(col_name)`?

---

    Code
      metric_set(demographic_parity, equal_opportunity, roc_auc)
    Condition
      Error in `metric_set()`:
      ! The inputs `demographic_parity` and `equal_opportunity` are groupwise metric (`?yardstick::new_groupwise_metric()`) factories and must be passed a data-column before addition to a metric set.
      i Did you mean to type e.g. `demographic_parity(col_name)`?

# propagates 'caused by' error message when specifying the wrong column name

    Code
      set(two_class_example, truth, Class1, estimate = predicted, case_weights = weight)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `accuracy()`.
      Caused by error:
      ! Can't select columns that don't exist.
      x Column `weight` doesn't exist.

# metric_set() errors on empty input

    Code
      metric_set()
    Condition
      Error in `metric_set()`:
      ! At least 1 function must be supplied to `...`.

