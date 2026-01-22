# Mean absolute scaled error

Calculate the mean absolute scaled error. This metric is *scale
independent* and *symmetric*. It is generally used for comparing
forecast error in time series settings. Due to the time series nature of
this metric, it is necessary to order observations in ascending order by
time.

## Usage

``` r
mase(data, ...)

# S3 method for class 'data.frame'
mase(
  data,
  truth,
  estimate,
  m = 1L,
  mae_train = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)

mase_vec(
  truth,
  estimate,
  m = 1L,
  mae_train = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by the `truth` and
  `estimate` arguments.

- ...:

  Not currently used.

- truth:

  The column identifier for the true results (that is `numeric`). This
  should be an unquoted column name although this argument is passed by
  expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `numeric`
  vector.

- estimate:

  The column identifier for the predicted results (that is also
  `numeric`). As with `truth` this can be specified different ways but
  the primary method is to use an unquoted variable name. For `_vec()`
  functions, a `numeric` vector.

- m:

  An integer value of the number of lags used to calculate the in-sample
  seasonal naive error. The default is used for non-seasonal time
  series. If each observation was at the daily level and the data showed
  weekly seasonality, then `m = 7L` would be a reasonable choice for a
  7-day seasonal naive calculation.

- mae_train:

  A numeric value which allows the user to provide the in-sample
  seasonal naive mean absolute error. If this value is not provided,
  then the out-of-sample seasonal naive mean absolute error will be
  calculated from `truth` and will be used instead.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in `data`. For
  `_vec()` functions, a numeric vector,
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
  or
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `mase_vec()`, a single `numeric` value (or `NA`).

## Details

`mase()` is different from most numeric metrics. The original
implementation of `mase()` calls for using the *in-sample* naive mean
absolute error to compute scaled errors with. It uses this instead of
the out-of-sample error because there is a chance that the out-of-sample
error cannot be computed when forecasting a very short horizon (i.e. the
out of sample size is only 1 or 2). However, `yardstick` only knows
about the out-of-sample `truth` and `estimate` values. Because of this,
the out-of-sample error is used in the computation by default. If the
in-sample naive mean absolute error is required and known, it can be
passed through in the `mae_train` argument and it will be used instead.
If the in-sample data is available, the naive mean absolute error can
easily be computed with `mae(data, truth, lagged_truth)`.

MASE is a metric that should be minimized. The output ranges from 0 to
∞, with 0 indicating perfect predictions.

The formula for MASE is:

\$\$\text{MASE} = \frac{1}{n} \sum\_{i=1}^{n} \frac{\|\text{truth}\_i -
\text{estimate}\_i\|}{\text{MAE}\_{naive}}\$\$

## References

Rob J. Hyndman (2006). ANOTHER LOOK AT FORECAST-ACCURACY METRICS FOR
INTERMITTENT DEMAND. *Foresight*, 4, 46.

## See also

Other numeric metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

Other accuracy metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Author

Alex Hallam

## Examples

``` r
# Supply truth and predictions as bare column names
mase(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mase    standard        3.56

library(dplyr)

set.seed(1234)
size <- 100
times <- 10

# create 10 resamples
solubility_resampled <- bind_rows(
  replicate(
    n = times,
    expr = sample_n(solubility_test, size, replace = TRUE),
    simplify = FALSE
  ),
  .id = "resample"
)

# Compute the metric by group
metric_results <- solubility_resampled |>
  group_by(resample) |>
  mase(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 1        mase    standard       0.256
#>  2 10       mase    standard       0.240
#>  3 2        mase    standard       0.238
#>  4 3        mase    standard       0.219
#>  5 4        mase    standard       0.229
#>  6 5        mase    standard       0.261
#>  7 6        mase    standard       0.217
#>  8 7        mase    standard       0.267
#>  9 8        mase    standard       0.216
#> 10 9        mase    standard       0.251

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.240
```
