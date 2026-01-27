# Integrated survival metrics

Integrated survival metrics summarize model performance across multiple
evaluation time points into a single value by integrating dynamic
survival metrics over time.

## Input requirements

- `truth`: a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object

- `...`: list column of data frames containing `.eval_time`,
  `.pred_survival`, and `.weight_censored` columns

## Available metrics

- [`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md):

  Direction: minimize. Range: \[0, 1\]

## See also

[dynamic-survival-metrics](https://yardstick.tidymodels.org/dev/reference/dynamic-survival-metrics.md)
for time-dependent survival metrics

[static-survival-metrics](https://yardstick.tidymodels.org/dev/reference/static-survival-metrics.md)
for static survival metrics

[linear-pred-survival-metrics](https://yardstick.tidymodels.org/dev/reference/linear-pred-survival-metrics.md)
for linear predictor survival metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
library(dplyr)
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
  brier_survival_integrated(truth = surv_obj, .pred)
#> # A tibble: 1 × 3
#>   .metric                   .estimator .estimate
#>   <chr>                     <chr>          <dbl>
#> 1 brier_survival_integrated standard       0.158
```
