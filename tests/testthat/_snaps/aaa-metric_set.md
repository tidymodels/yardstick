# print metric_set works

    Code
      metric_set(accuracy)
    Output
      A metric set, consisting of:
      - `accuracy()`, a class metric | direction: maximize

---

    Code
      metric_set(roc_auc)
    Output
      A metric set, consisting of:
      - `roc_auc()`, a probability metric | direction: maximize

---

    Code
      metric_set(ranked_prob_score)
    Output
      A metric set, consisting of:
      - `ranked_prob_score()`, a ordered probability metric | direction: minimize

---

    Code
      metric_set(rmse)
    Output
      A metric set, consisting of:
      - `rmse()`, a numeric metric | direction: minimize

---

    Code
      metric_set(concordance_survival)
    Output
      A metric set, consisting of:
      - `concordance_survival()`, a static survival metric | direction: maximize

---

    Code
      metric_set(brier_survival)
    Output
      A metric set, consisting of:
      - `brier_survival()`, a dynamic survival metric | direction: minimize

---

    Code
      metric_set(brier_survival_integrated)
    Output
      A metric set, consisting of:
      - `brier_survival_integrated()`, a integrated survival metric | direction:
      minimize

---

    Code
      metric_set(royston_survival)
    Output
      A metric set, consisting of:
      - `royston_survival()`, a linear predictor survival metric | direction:
      maximize

---

    Code
      metric_set(weighted_interval_score)
    Output
      A metric set, consisting of:
      - `weighted_interval_score()`, a quantile metric | direction: minimize

---

    Code
      metric_set(accuracy, roc_auc, ranked_prob_score)
    Output
      A metric set, consisting of:
      - `accuracy()`, a class metric                        | direction: maximize
      - `roc_auc()`, a probability metric                   | direction: maximize
      - `ranked_prob_score()`, a ordered probability metric | direction: minimize

---

    Code
      metric_set(concordance_survival, brier_survival, brier_survival_integrated,
        royston_survival)
    Output
      A metric set, consisting of:
      - `concordance_survival()`, a static survival metric          | direction:
      maximize
      - `brier_survival()`, a dynamic survival metric               | direction:
      minimize
      - `brier_survival_integrated()`, a integrated survival metric | direction:
      minimize
      - `royston_survival()`, a linear predictor survival metric    | direction:
      maximize

# metric_set() errors on bad input

    Code
      metric_set("x")
    Condition
      Error in `metric_set()`:
      ! All inputs to `metric_set()` must be functions. These inputs are not: 1.

---

    Code
      metric_set(rmse, "x")
    Condition
      Error in `metric_set()`:
      ! All inputs to `metric_set()` must be functions. These inputs are not: 2.

# metric_set() errors on empty input

    Code
      metric_set()
    Condition
      Error in `metric_set()`:
      ! At least 1 function must be supplied to `...`.

# metric_set() errors on mixing incombatible metrics

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

---

    Code
      metric_set(rmse, accuracy, brier_survival)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - numeric (rmse)
      - class (accuracy)
      - dynamic_survival (brier_survival)

---

    Code
      metric_set(rmse, accuracy, brier_survival, weighted_interval_score)
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - numeric (rmse)
      - class (accuracy)
      - dynamic_survival (brier_survival)
      - quantile (weighted_interval_score)

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

# errors informatively when `estimate` is not named for class metrics

    Code
      set(two_class_example, truth, predicted)
    Condition
      Error in `set()`:
      ! `estimate` is required for class metrics but was not provided.
      i The `estimate` argument must be named because it comes after `...`.
      i Example: `my_metrics(data, truth, estimate = my_column)`

# errors informatively when `estimate` is not named for survival metrics

    Code
      set(lung_surv, surv_obj, .pred_time)
    Condition
      Error in `set()`:
      ! `estimate` is required for static or linear predictor survival metrics but was not provided.
      i The `estimate` argument must be named because it comes after `...`.
      i Example: `my_metrics(data, truth, estimate = my_column)`

