# Concordance correlation coefficient

Calculate the concordance correlation coefficient.

## Usage

``` r
ccc(data, ...)

# S3 method for class 'data.frame'
ccc(
  data,
  truth,
  estimate,
  bias = FALSE,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)

ccc_vec(truth, estimate, bias = FALSE, na_rm = TRUE, case_weights = NULL, ...)
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

- bias:

  A `logical`; should the biased estimate of variance be used (as is Lin
  (1989))?

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

For `ccc_vec()`, a single `numeric` value (or `NA`).

## Details

`ccc()` is a metric of both consistency/correlation and accuracy, while
metrics such as
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md) are
strictly for accuracy and metrics such as
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md) are
strictly for consistency/correlation

CCC is a metric that should be maximized. The output ranges from -1 to
1, with 1 indicating perfect agreement.

The formula for CCC is:

\$\$\text{CCC} = \frac{2 \cdot \text{cov}(\text{truth},
\text{estimate})}{\text{var}(\text{truth}) +
\text{var}(\text{estimate}) + (\bar{\text{truth}} -
\bar{\text{estimate}})^2}\$\$

## References

Lin, L. (1989). A concordance correlation coefficient to evaluate
reproducibility. *Biometrics*, 45 (1), 255-268.

Nickerson, C. (1997). A note on "A concordance correlation coefficient
to evaluate reproducibility". *Biometrics*, 53(4), 1503-1507.

## See also

[All numeric
metrics](https://yardstick.tidymodels.org/dev/reference/numeric-metrics.md)

Other numeric metrics:
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
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

Other consistency metrics:
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md)

Other accuracy metrics:
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
[`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

## Author

Max Kuhn

## Examples

``` r
# Supply truth and predictions as bare column names
ccc(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 ccc     standard       0.937

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
  ccc(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 1        ccc     standard       0.935
#>  2 10       ccc     standard       0.937
#>  3 2        ccc     standard       0.943
#>  4 3        ccc     standard       0.956
#>  5 4        ccc     standard       0.944
#>  6 5        ccc     standard       0.925
#>  7 6        ccc     standard       0.933
#>  8 7        ccc     standard       0.922
#>  9 8        ccc     standard       0.955
#> 10 9        ccc     standard       0.940

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1        0.939
```
