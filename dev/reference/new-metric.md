# Construct a new metric function

These functions provide convenient wrappers to create the three types of
metric functions in yardstick: numeric metrics, class metrics, and class
probability metrics. They add a metric-specific class to `fn` and attach
a `direction` attribute. These features are used by
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
and by [tune](https://tune.tidymodels.org/) when model tuning.

See [Custom performance
metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
information about creating custom metrics.

## Usage

``` r
new_class_metric(fn, direction)

new_prob_metric(fn, direction)

new_ordered_prob_metric(fn, direction)

new_numeric_metric(fn, direction)

new_dynamic_survival_metric(fn, direction)

new_integrated_survival_metric(fn, direction)

new_static_survival_metric(fn, direction)

new_linear_pred_survival_metric(fn, direction)
```

## Arguments

- fn:

  A function. The metric function to attach a metric-specific class and
  `direction` attribute to.

- direction:

  A string. One of:

  - `"maximize"`

  - `"minimize"`

  - `"zero"`
