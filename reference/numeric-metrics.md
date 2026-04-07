# Numeric metrics

Numeric metrics evaluate regression predictions where both `truth` and
`estimate` are numeric. These metrics measure how close predicted values
are to the true values.

## Input requirements

- `truth`: numeric

- `estimate`: numeric

## Available metrics

- [`ccc()`](https://yardstick.tidymodels.org/reference/ccc.md):

  Direction: maximize. Range: \[-1, 1\]

- [`gini_coef()`](https://yardstick.tidymodels.org/reference/gini_coef.md):

  Direction: maximize. Range: \[0, 1\]

- [`huber_loss()`](https://yardstick.tidymodels.org/reference/huber_loss.md):

  Direction: minimize. Range: \[0, Inf\]

- [`huber_loss_pseudo()`](https://yardstick.tidymodels.org/reference/huber_loss_pseudo.md):

  Direction: minimize. Range: \[0, Inf\]

- [`iic()`](https://yardstick.tidymodels.org/reference/iic.md):

  Direction: maximize. Range: \[-1, 1\]

- [`mae()`](https://yardstick.tidymodels.org/reference/mae.md):

  Direction: minimize. Range: \[0, Inf\]

- [`mape()`](https://yardstick.tidymodels.org/reference/mape.md):

  Direction: minimize. Range: \[0, Inf\]

- [`mase()`](https://yardstick.tidymodels.org/reference/mase.md):

  Direction: minimize. Range: \[0, Inf\]

- [`mpe()`](https://yardstick.tidymodels.org/reference/mpe.md):

  Direction: zero. Range: \[-Inf, Inf\]

- [`msd()`](https://yardstick.tidymodels.org/reference/msd.md):

  Direction: zero. Range: \[-Inf, Inf\]

- [`mse()`](https://yardstick.tidymodels.org/reference/mse.md):

  Direction: minimize. Range: \[0, Inf\]

- [`poisson_log_loss()`](https://yardstick.tidymodels.org/reference/poisson_log_loss.md):

  Direction: minimize. Range: \[0, Inf\]

- [`rmse()`](https://yardstick.tidymodels.org/reference/rmse.md):

  Direction: minimize. Range: \[0, Inf\]

- [`rmse_relative()`](https://yardstick.tidymodels.org/reference/rmse_relative.md):

  Direction: minimize. Range: \[0, Inf\]

- [`rpd()`](https://yardstick.tidymodels.org/reference/rpd.md):

  Direction: maximize. Range: \[0, Inf\]

- [`rpiq()`](https://yardstick.tidymodels.org/reference/rpiq.md):

  Direction: maximize. Range: \[0, Inf\]

- [`rsq()`](https://yardstick.tidymodels.org/reference/rsq.md):

  Direction: maximize. Range: \[-Inf, 1\]

- [`rsq_trad()`](https://yardstick.tidymodels.org/reference/rsq_trad.md):

  Direction: maximize. Range: \[0, 1\]

- [`smape()`](https://yardstick.tidymodels.org/reference/smape.md):

  Direction: minimize. Range: \[0, 100\]

## See also

[quantile-metrics](https://yardstick.tidymodels.org/reference/quantile-metrics.md)
for quantile prediction metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
data("solubility_test")

head(solubility_test)
#>   solubility prediction
#> 1       0.93  0.3677522
#> 2       0.85 -0.1503220
#> 3       0.81 -0.5051844
#> 4       0.74  0.5398116
#> 5       0.61 -0.4792718
#> 6       0.58  0.7377222

rmse(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard       0.722
```
