# Developer function for checking inputs in new metrics

`check_numeric_metric()`, `check_class_metric()`, and
`check_prob_metric()` are useful alongside
[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
for implementing new custom metrics.
[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
call the metric function inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
These functions perform checks on the inputs in accordance with the type
of metric that is used.

## Usage

``` r
check_numeric_metric(truth, estimate, case_weights, call = caller_env())

check_class_metric(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
)

check_prob_metric(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
)

check_ordered_prob_metric(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
)

check_dynamic_survival_metric(
  truth,
  estimate,
  case_weights,
  call = caller_env()
)

check_static_survival_metric(
  truth,
  estimate,
  case_weights,
  call = caller_env()
)

check_linear_pred_survival_metric(
  truth,
  estimate,
  case_weights,
  call = caller_env()
)
```

## Arguments

- truth:

  The realized vector of `truth`.

  - For `check_numeric_metric()`, a numeric vector.

  - For `check_class_metric()`, a factor.

  - For `check_prob_metric()`, a factor.

  - For `check_ordered_prob_metric()`, an ordered factor.

  - For `check_dynamic_survival_metric()`, a Surv object.

  - For `check_static_survival_metric()`, a Surv object.

- estimate:

  The realized `estimate` result.

  - For `check_numeric_metric()`, a numeric vector.

  - For `check_class_metric()`, a factor.

  - For `check_prob_metric()`, a numeric vector for binary `truth`, a
    numeric matrix for multic-class `truth`.

  - For `check_ordered_prob_metric()`, a numeric vector for binary
    `truth`, a numeric matrix for multic-class `truth`.

  - For `check_dynamic_survival_metric()`, list-column of data.frames.

  - For `check_static_survival_metric()`, a numeric vector.

- case_weights:

  The realized case weights, as a numeric vector. This must be the same
  length as `truth`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- estimator:

  This can either be `NULL` for the default auto-selection of averaging
  (`"binary"` or `"macro"`), or a single character to pass along to the
  metric implementation describing the kind of averaging to use.

## See also

[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
