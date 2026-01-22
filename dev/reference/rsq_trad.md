# R squared - traditional

Calculate the coefficient of determination using the traditional
definition of R squared using sum of squares. For a measure of R squared
that is strictly between (0, 1), see
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md).

## Usage

``` r
rsq_trad(data, ...)

# S3 method for class 'data.frame'
rsq_trad(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

rsq_trad_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `rsq_trad_vec()`, a single `numeric` value (or `NA`).

## Details

The two estimates for the coefficient of determination,
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md) and
`rsq_trad()`, differ by their formula. The former guarantees a value on
(0, 1) while the latter can generate inaccurate values when the model is
non-informative (see the examples). Both are measures of
consistency/correlation and not of accuracy.

Traditional R squared is a metric that should be maximized. The output
ranges from 0 to 1, with 1 indicating perfect predictions. Negative
values can occur when the model is non-informative.

The formula for traditional R squared is:

\$\$\text{rsq\\trad} = 1 - \frac{SS\_{res}}{SS\_{tot}} = 1 -
\frac{\sum\_{i=1}^{n}(\text{truth}\_i -
\text{estimate}\_i)^2}{\sum\_{i=1}^{n}(\text{truth}\_i -
\bar{\text{truth}})^2}\$\$

## References

Kvalseth. Cautionary note about \\R^2\\. American Statistician (1985)
vol. 39 (4) pp. 279-285.

## See also

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
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

Other consistency metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md)

## Author

Max Kuhn

## Examples

``` r
# Supply truth and predictions as bare column names
rsq_trad(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 rsq_trad standard       0.879

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
  rsq_trad(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 1        rsq_trad standard       0.870
#>  2 10       rsq_trad standard       0.878
#>  3 2        rsq_trad standard       0.891
#>  4 3        rsq_trad standard       0.913
#>  5 4        rsq_trad standard       0.889
#>  6 5        rsq_trad standard       0.857
#>  7 6        rsq_trad standard       0.872
#>  8 7        rsq_trad standard       0.852
#>  9 8        rsq_trad standard       0.915
#> 10 9        rsq_trad standard       0.883

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.882
# With uninformitive data, the traditional version of R^2 can return
# negative values.
set.seed(2291)
solubility_test$randomized <- sample(solubility_test$prediction)
rsq(solubility_test, solubility, randomized)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rsq     standard     0.00199
rsq_trad(solubility_test, solubility, randomized)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 rsq_trad standard       -1.01
```
