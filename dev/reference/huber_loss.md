# Huber loss

Calculate the Huber loss, a loss function used in robust regression.
This loss function is less sensitive to outliers than
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md). This
function is quadratic for small residual values and linear for large
residual values.

## Usage

``` r
huber_loss(data, ...)

# S3 method for class 'data.frame'
huber_loss(
  data,
  truth,
  estimate,
  delta = 1,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)

huber_loss_vec(
  truth,
  estimate,
  delta = 1,
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

- delta:

  A single `numeric` value. Defines the boundary where the loss function
  transitions from quadratic to linear. Defaults to 1.

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

For `huber_loss_vec()`, a single `numeric` value (or `NA`).

## Details

Huber loss is a metric that should be minimized. The output ranges from
0 to ∞, with 0 indicating perfect predictions.

The formula for Huber loss is:

\$\$L\_\delta = \begin{cases} \frac{1}{2} a^2 & \text{if } \|a\| \le
\delta \\ \delta (\|a\| - \frac{1}{2} \delta) & \text{otherwise}
\end{cases}\$\$

where \\a = \text{truth}\_i - \text{estimate}\_i\\.

## References

Huber, P. (1964). Robust Estimation of a Location Parameter. *Annals of
Statistics*, 53 (1), 73-101.

## See also

Other numeric metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
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
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Author

James Blair

## Examples

``` r
# Supply truth and predictions as bare column names
huber_loss(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric    .estimator .estimate
#>   <chr>      <chr>          <dbl>
#> 1 huber_loss standard       0.234

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
  huber_loss(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric    .estimator .estimate
#>    <chr>    <chr>      <chr>          <dbl>
#>  1 1        huber_loss standard       0.215
#>  2 10       huber_loss standard       0.212
#>  3 2        huber_loss standard       0.229
#>  4 3        huber_loss standard       0.197
#>  5 4        huber_loss standard       0.249
#>  6 5        huber_loss standard       0.208
#>  7 6        huber_loss standard       0.293
#>  8 7        huber_loss standard       0.268
#>  9 8        huber_loss standard       0.190
#> 10 9        huber_loss standard       0.218

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.228
```
