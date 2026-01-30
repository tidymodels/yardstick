# Normalized Gini coefficient

Compute the normalized Gini coefficient, which measures the ranking
ability of a regression model based on the Lorenz curve. This metric is
useful for evaluating models that predict risk or loss costs, such as
insurance pricing models.

## Usage

``` r
gini_coef(data, ...)

# S3 method for class 'data.frame'
gini_coef(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

gini_coef_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `gini_coef_vec()`, a single `numeric` value (or `NA`).

## Details

The normalized Gini coefficient is a metric that should be maximized.
The output ranges from 0 to 1, with 1 indicating perfect ranking ability
where predicted values perfectly rank the true values.

The Gini coefficient is calculated from the Lorenz curve, which plots
the cumulative proportion of the total truth values against the
cumulative proportion of observations when sorted by predicted values.
The raw Gini is the area between the Lorenz curve and the diagonal line
of equality. The normalized Gini divides this by the maximum possible
Gini (achieved when observations are sorted by the true values).

The formula is:

\$\$\text{Normalized Gini} =
\frac{G(\text{estimate})}{G(\text{truth})}\$\$

where \\G(x)\\ is the Gini coefficient when sorting by \\x\\.

Note that `gini_coef()` is a regression metric based on ranking,
distinct from
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md)
which is a classification metric.

Unlike many other metrics, `gini_coef()` is not symmetric with respect
to `truth` and `estimate`. The `estimate` values determine the sorting
order, while the `truth` values are accumulated along the Lorenz curve.
Swapping them will produce different results.

When the true values are constant (zero variance), the Gini coefficient
is undefined and `NA` is returned with a warning.

## See also

[All numeric
metrics](https://yardstick.tidymodels.org/dev/reference/numeric-metrics.md)

Other numeric metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`mse()`](https://yardstick.tidymodels.org/dev/reference/mse.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Examples

``` r
# Supply truth and predictions as bare column names
gini_coef(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 gini_coef standard       0.935

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
  gini_coef(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric   .estimator .estimate
#>    <chr>    <chr>     <chr>          <dbl>
#>  1 1        gini_coef standard       0.929
#>  2 10       gini_coef standard       0.946
#>  3 2        gini_coef standard       0.940
#>  4 3        gini_coef standard       0.945
#>  5 4        gini_coef standard       0.946
#>  6 5        gini_coef standard       0.923
#>  7 6        gini_coef standard       0.931
#>  8 7        gini_coef standard       0.921
#>  9 8        gini_coef standard       0.951
#> 10 9        gini_coef standard       0.936

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.937
```
