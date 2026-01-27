# Quantile metrics

Quantile metrics evaluate predictions that consist of predicted
quantiles rather than point predictions. These metrics assess the
accuracy and calibration of distributional forecasts.

## Input requirements

- `truth`: numeric

- `estimate`:
  [hardhat::quantile_pred](https://hardhat.tidymodels.org/reference/quantile_pred.html)
  object

## Available metrics

- [`weighted_interval_score()`](https://yardstick.tidymodels.org/dev/reference/weighted_interval_score.md):

  Direction: minimize. Range: \[0, Inf\]

## See also

[numeric-metrics](https://yardstick.tidymodels.org/dev/reference/numeric-metrics.md)
for point prediction metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
library(hardhat)

df <- data.frame(
  preds = quantile_pred(rbind(1:4, 8:11), c(0.2, 0.4, 0.6, 0.8)),
  truth = c(3.3, 7.1)
)

df
#>   preds truth
#> 1 [2.5]   3.3
#> 2 [9.5]   7.1

weighted_interval_score(df, truth, preds)
#> # A tibble: 1 × 3
#>   .metric                 .estimator .estimate
#>   <chr>                   <chr>          <dbl>
#> 1 weighted_interval_score standard        1.28
```
