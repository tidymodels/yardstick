# Get all metrics of a given type

`get_metrics()` returns a
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
containing all yardstick metrics of the specified type(s).

## Usage

``` r
get_metrics(type)
```

## Arguments

- type:

  A character vector of metric types. Valid types are: `"class"`,
  `"prob"`, `"ordered_prob"`, `"numeric"`, `"dynamic_survival"`,
  `"integrated_survival"`, `"static_survival"`,
  `"linear_pred_survival"`, and `"quantile"`.

  More than 1 type can be selected but you are constrained by which
  metric types
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  allows to be combined. This means that
  `get_metrics(c("class", "prob"))` will run without error, but
  `get_metrics(c("class", "numeric"))` will return an error because you
  can't combine `"class"` and `"numeric"` metrics.

## Value

A
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
containing all metrics of the specified type(s).

## See also

[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)

## Examples

``` r
get_metrics("numeric")
#> A metric set, consisting of:
#> - `ccc()`, a numeric metric               | direction: maximize
#> - `huber_loss()`, a numeric metric        | direction: minimize
#> - `huber_loss_pseudo()`, a numeric metric | direction: minimize
#> - `iic()`, a numeric metric               | direction: maximize
#> - `mae()`, a numeric metric               | direction: minimize
#> - `mape()`, a numeric metric              | direction: minimize
#> - `mase()`, a numeric metric              | direction: minimize
#> - `mpe()`, a numeric metric               | direction: zero
#> - `msd()`, a numeric metric               | direction: zero
#> - `mse()`, a numeric metric               | direction: minimize
#> - `poisson_log_loss()`, a numeric metric  | direction: minimize
#> - `rmse()`, a numeric metric              | direction: minimize
#> - `rmse_relative()`, a numeric metric     | direction: minimize
#> - `rpd()`, a numeric metric               | direction: maximize
#> - `rpiq()`, a numeric metric              | direction: maximize
#> - `rsq()`, a numeric metric               | direction: maximize
#> - `rsq_trad()`, a numeric metric          | direction: maximize
#> - `smape()`, a numeric metric             | direction: minimize

get_metrics("class")
#> A metric set, consisting of:
#> - `accuracy()`, a class metric             | direction: maximize
#> - `bal_accuracy()`, a class metric         | direction: maximize
#> - `detection_prevalence()`, a class metric | direction: maximize
#> - `f_meas()`, a class metric               | direction: maximize
#> - `fall_out()`, a class metric             | direction: minimize
#> - `j_index()`, a class metric              | direction: maximize
#> - `kap()`, a class metric                  | direction: maximize
#> - `mcc()`, a class metric                  | direction: maximize
#> - `miss_rate()`, a class metric            | direction: minimize
#> - `npv()`, a class metric                  | direction: maximize
#> - `ppv()`, a class metric                  | direction: maximize
#> - `precision()`, a class metric            | direction: maximize
#> - `recall()`, a class metric               | direction: maximize
#> - `sens()`, a class metric                 | direction: maximize
#> - `sensitivity()`, a class metric          | direction: maximize
#> - `spec()`, a class metric                 | direction: maximize
#> - `specificity()`, a class metric          | direction: maximize

# Get multiple types at once
get_metrics(c("class", "prob"))
#> A metric set, consisting of:
#> - `accuracy()`, a class metric                  | direction: maximize
#> - `average_precision()`, a probability metric   | direction: maximize
#> - `bal_accuracy()`, a class metric              | direction: maximize
#> - `brier_class()`, a probability metric         | direction: minimize
#> - `classification_cost()`, a probability metric | direction: minimize
#> - `detection_prevalence()`, a class metric      | direction: maximize
#> - `f_meas()`, a class metric                    | direction: maximize
#> - `fall_out()`, a class metric                  | direction: minimize
#> - `gain_capture()`, a probability metric        | direction: maximize
#> - `j_index()`, a class metric                   | direction: maximize
#> - `kap()`, a class metric                       | direction: maximize
#> - `mcc()`, a class metric                       | direction: maximize
#> - `miss_rate()`, a class metric                 | direction: minimize
#> - `mn_log_loss()`, a probability metric         | direction: minimize
#> - `npv()`, a class metric                       | direction: maximize
#> - `ppv()`, a class metric                       | direction: maximize
#> - `pr_auc()`, a probability metric              | direction: maximize
#> - `precision()`, a class metric                 | direction: maximize
#> - `recall()`, a class metric                    | direction: maximize
#> - `roc_auc()`, a probability metric             | direction: maximize
#> - `roc_aunp()`, a probability metric            | direction: maximize
#> - `roc_aunu()`, a probability metric            | direction: maximize
#> - `sens()`, a class metric                      | direction: maximize
#> - `sensitivity()`, a class metric               | direction: maximize
#> - `spec()`, a class metric                      | direction: maximize
#> - `specificity()`, a class metric               | direction: maximize
```
