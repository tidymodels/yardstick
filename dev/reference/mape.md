# Mean absolute percent error

Calculate the mean absolute percentage error. This metric is in
*relative units*.

## Usage

``` r
mape(data, ...)

# S3 method for class 'data.frame'
mape(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

mape_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `mape_vec()`, a single `numeric` value (or `NA`).

## Details

Note that a value of `Inf` is returned for `mape()` when the observed
value is negative.

MAPE is a metric that should be minimized. The output ranges from 0 to
∞, with 0 indicating perfect predictions.

The formula for MAPE is:

\$\$\text{MAPE} = \frac{100}{n} \sum\_{i=1}^{n} \left\|
\frac{\text{truth}\_i - \text{estimate}\_i}{\text{truth}\_i}
\right\|\$\$

## See also

[All numeric
metrics](https://yardstick.tidymodels.org/dev/reference/numeric-metrics.md)

Other numeric metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
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
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Author

Max Kuhn

## Examples

``` r
# Supply truth and predictions as bare column names
mape(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mape    standard         Inf

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
  mape(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 1        mape    standard       Inf  
#>  2 10       mape    standard       Inf  
#>  3 2        mape    standard       Inf  
#>  4 3        mape    standard        28.5
#>  5 4        mape    standard        95.3
#>  6 5        mape    standard       Inf  
#>  7 6        mape    standard        73.1
#>  8 7        mape    standard       Inf  
#>  9 8        mape    standard       Inf  
#> 10 9        mape    standard        37.9

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1          Inf
```
