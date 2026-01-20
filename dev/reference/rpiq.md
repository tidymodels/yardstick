# Ratio of performance to inter-quartile

These functions are appropriate for cases where the model outcome is a
numeric. The ratio of performance to deviation
([`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md)) and
the ratio of performance to inter-quartile (`rpiq()`) are both measures
of consistency/correlation between observed and predicted values (and
not of accuracy).

## Usage

``` r
rpiq(data, ...)

# S3 method for class 'data.frame'
rpiq(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

rpiq_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For
[`rpd_vec()`](https://yardstick.tidymodels.org/dev/reference/rpd.md), a
single `numeric` value (or `NA`).

## Details

RPIQ is a metric that should be maximized. The output ranges from 0 to
∞, with higher values indicating better model performance.

## References

Williams, P.C. (1987) Variables affecting near-infrared reflectance
spectroscopic analysis. In: Near Infrared Technology in the Agriculture
and Food Industries. 1st Ed. P.Williams and K.Norris, Eds. Am. Cereal
Assoc. Cereal Chem., St. Paul, MN.

Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger, J.M. and
McBratney, A., (2010). Critical review of chemometric indicators
commonly used for assessing the quality of the prediction of soil
attributes by NIR spectroscopy. TrAC Trends in Analytical Chemistry,
29(9), pp.1073-1081.

## See also

The closely related deviation metric:
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md)

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
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md),
[`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)

Other consistency metrics:
[`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md),
[`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md),
[`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md),
[`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md)

## Author

Pierre Roudier

## Examples

``` r
# Supply truth and predictions as bare column names
rpd(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rpd     standard        2.88

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
  rpd(solubility, prediction)

metric_results
#> # A tibble: 10 × 4
#>    resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 1        rpd     standard        2.78
#>  2 10       rpd     standard        2.87
#>  3 2        rpd     standard        3.04
#>  4 3        rpd     standard        3.41
#>  5 4        rpd     standard        3.02
#>  6 5        rpd     standard        2.66
#>  7 6        rpd     standard        2.81
#>  8 7        rpd     standard        2.61
#>  9 8        rpd     standard        3.45
#> 10 9        rpd     standard        2.93

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
#> # A tibble: 1 × 1
#>   avg_estimate
#>          <dbl>
#> 1         2.96
```
