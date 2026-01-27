# Linear predictor survival metrics

Linear predictor survival metrics evaluate survival model predictions
based on linear predictors (log-hazard or log-risk scores).

## Input requirements

- `truth`: a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object

- `estimate`: numeric (linear predictor values)

## Available metrics

- [`royston_survival()`](https://yardstick.tidymodels.org/dev/reference/royston_survival.md):

  Direction: maximize. Range: \[0, 1\]

## See also

[dynamic-survival-metrics](https://yardstick.tidymodels.org/dev/reference/dynamic-survival-metrics.md)
for time-dependent survival metrics

[integrated-survival-metrics](https://yardstick.tidymodels.org/dev/reference/integrated-survival-metrics.md)
for integrated survival metrics

[static-survival-metrics](https://yardstick.tidymodels.org/dev/reference/static-survival-metrics.md)
for static survival metrics

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

lung_surv |>
  royston_survival(truth = surv_obj, estimate = .pred_linear_pred)
#> # A tibble: 1 × 3
#>   .metric          .estimator .estimate
#>   <chr>            <chr>          <dbl>
#> 1 royston_survival standard       0.116
```
