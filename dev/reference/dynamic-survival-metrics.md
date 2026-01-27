# Dynamic survival metrics

Dynamic survival metrics evaluate time-dependent survival predictions,
producing one metric value per evaluation time point. These metrics
assess predicted survival probabilities at specific time points.

## Input requirements

- `truth`: a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object

- `...`: list column of data frames containing `.eval_time`,
  `.pred_survival`, and `.weight_censored` columns

## Available metrics

- [`brier_survival()`](https://yardstick.tidymodels.org/dev/reference/brier_survival.md):

  Direction: minimize. Range: \[0, 1\]

- [`roc_auc_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_auc_survival.md):

  Direction: maximize. Range: \[0, 1\]

## See also

[integrated-survival-metrics](https://yardstick.tidymodels.org/dev/reference/integrated-survival-metrics.md)
for integrated survival metrics

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
  brier_survival(truth = surv_obj, .pred)
#> # A tibble: 5 × 4
#>   .metric        .estimator .eval_time .estimate
#>   <chr>          <chr>           <dbl>     <dbl>
#> 1 brier_survival standard          100     0.109
#> 2 brier_survival standard          200     0.194
#> 3 brier_survival standard          300     0.219
#> 4 brier_survival standard          400     0.222
#> 5 brier_survival standard          500     0.197
```
