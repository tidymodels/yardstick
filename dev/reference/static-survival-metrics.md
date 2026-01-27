# Static survival metrics

Static survival metrics evaluate survival predictions that do not depend
on a specific evaluation time point. These metrics typically compare
predicted risk scores or survival times against observed outcomes.

## Input requirements

- `truth`: a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object

- `estimate`: numeric (predicted time or risk score)

## Available metrics

- [`concordance_survival()`](https://yardstick.tidymodels.org/dev/reference/concordance_survival.md):

  Direction: maximize. Range: \[0, 1\]

## See also

[dynamic-survival-metrics](https://yardstick.tidymodels.org/dev/reference/dynamic-survival-metrics.md)
for time-dependent survival metrics

[integrated-survival-metrics](https://yardstick.tidymodels.org/dev/reference/integrated-survival-metrics.md)
for integrated survival metrics

[linear-pred-survival-metrics](https://yardstick.tidymodels.org/dev/reference/linear-pred-survival-metrics.md)
for linear predictor survival metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
data("lung_surv")

head(lung_surv)
#> # A tibble: 6 × 4
#>   .pred            .pred_time surv_obj .pred_linear_pred
#>   <list>                <dbl>   <Surv>             <dbl>
#> 1 <tibble [5 × 5]>       324.     306               5.78
#> 2 <tibble [5 × 5]>       476.     455               6.17
#> 3 <tibble [5 × 5]>       521.    1010+              6.26
#> 4 <tibble [5 × 5]>       368.     210               5.91
#> 5 <tibble [5 × 5]>       506.     883               6.23
#> 6 <tibble [5 × 5]>       324.    1022+              5.78

concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time)
#> # A tibble: 1 × 3
#>   .metric              .estimator .estimate
#>   <chr>                <chr>          <dbl>
#> 1 concordance_survival standard       0.637
```
