# Index of ideality of correlation

Calculate the index of ideality of correlation. This metric has been
studied in QSPR/QSAR models as a good criterion for the predictive
potential of these models. It is highly dependent on the correlation
coefficient as well as the mean absolute error.

Note the application of IIC is useless under two conditions:

- When the negative mean absolute error and positive mean absolute error
  are both zero.

- When the outliers are symmetric. Since outliers are context dependent,
  please use your own checks to validate whether this restriction holds
  and whether the resulting IIC has interpretative value.

The IIC is seen as an alternative to the traditional correlation
coefficient and is in the same units as the original data.

## Usage

``` r
iic(data, ...)

# S3 method for class 'data.frame'
iic(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

iic_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `iic_vec()`, a single `numeric` value (or `NA`).

## Details

IIC is a metric that should be maximized. The output ranges from -1 to
1, with 1 indicating perfect agreement.

The formula for IIC is:

\$\$\text{IIC} = \text{corr}(\text{truth}, \text{estimate}) \cdot
\frac{\min(\text{MAE}^-, \text{MAE}^+)}{\max(\text{MAE}^-,
\text{MAE}^+)}\$\$

where \\\text{MAE}^-\\ and \\\text{MAE}^+\\ are the mean absolute errors
for negative and non-negative residuals, respectively.

## References

Toropova, A. and Toropov, A. (2017). "The index of ideality of
correlation. A criterion of predictability of QSAR models for skin
permeability?" *Science of the Total Environment*. 586: 466-472.

## See also

[All numeric
metrics](https://yardstick.tidymodels.org/dev/reference/numeric-metrics.md)

Other numeric metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`gini_coef()`](https://yardstick.tidymodels.org/dev/reference/gini_coef.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
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

Other accuracy metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md),
[`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md),
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md),
[`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md),
[`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md),
[`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md),
[`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md),
[`mse()`](https://yardstick.tidymodels.org/dev/reference/mse.md),
[`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md),
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Author

Joyce Cahoon

## Examples

``` r
# Supply truth and predictions as bare column names
iic(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 iic     standard       0.890

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
  iic(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 1        iic     standard       0.730
#>  2 10       iic     standard       0.731
#>  3 2        iic     standard       0.906
#>  4 3        iic     standard       0.877
#>  5 4        iic     standard       0.732
#>  6 5        iic     standard       0.821
#>  7 6        iic     standard       0.896
#>  8 7        iic     standard       0.867
#>  9 8        iic     standard       0.881
#> 10 9        iic     standard       0.748

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.819
```
