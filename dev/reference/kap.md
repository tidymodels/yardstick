# Kappa

Kappa is a similar measure to
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
but is normalized by the accuracy that would be expected by chance alone
and is very useful when one or more classes have large frequency
distributions.

## Usage

``` r
kap(data, ...)

# S3 method for class 'data.frame'
kap(
  data,
  truth,
  estimate,
  weighting = "none",
  na_rm = TRUE,
  case_weights = NULL,
  ...
)

kap_vec(
  truth,
  estimate,
  weighting = "none",
  na_rm = TRUE,
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  Either a `data.frame` containing the columns specified by the `truth`
  and `estimate` arguments, or a `table`/`matrix` where the true class
  results should be in the columns of the table.

- ...:

  Not currently used.

- truth:

  The column identifier for the true class results (that is a `factor`).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `factor`
  vector.

- estimate:

  The column identifier for the predicted class results (that is also
  `factor`). As with `truth` this can be specified different ways but
  the primary method is to use an unquoted variable name. For `_vec()`
  functions, a `factor` vector.

- weighting:

  A weighting to apply when computing the scores. One of: `"none"`,
  `"linear"`, or `"quadratic"`. Linear and quadratic weighting penalizes
  mis-predictions that are "far away" from the true value. Note that
  distance is judged based on the ordering of the levels in `truth` and
  `estimate`. It is recommended to provide ordered factors for `truth`
  and `estimate` to explicitly code the ordering, but this is not
  required.

  In the binary case, all 3 weightings produce the same value, since it
  is only ever possible to be 1 unit away from the true value.

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

For `kap_vec()`, a single `numeric` value (or `NA`).

## Multiclass

Kappa extends naturally to multiclass scenarios. Because of this, macro
and micro averaging are not implemented.

## References

Cohen, J. (1960). "A coefficient of agreement for nominal scales".
*Educational and Psychological Measurement*. 20 (1): 37-46.

Cohen, J. (1968). "Weighted kappa: Nominal scale agreement provision for
scaled disagreement or partial credit". *Psychological Bulletin*. 70
(4): 213-220.

## See also

Other class metrics:
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Author

Max Kuhn

Jon Harmon

## Examples

``` r
library(dplyr)
data("two_class_example")
data("hpc_cv")

# Two class
kap(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 kap     binary         0.675

# Multiclass
# kap() has a natural multiclass extension
hpc_cv |>
  filter(Resample == "Fold01") |>
  kap(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 kap     multiclass     0.533

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  kap(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   kap     multiclass     0.533
#>  2 Fold02   kap     multiclass     0.512
#>  3 Fold03   kap     multiclass     0.594
#>  4 Fold04   kap     multiclass     0.511
#>  5 Fold05   kap     multiclass     0.514
#>  6 Fold06   kap     multiclass     0.486
#>  7 Fold07   kap     multiclass     0.454
#>  8 Fold08   kap     multiclass     0.531
#>  9 Fold09   kap     multiclass     0.454
#> 10 Fold10   kap     multiclass     0.492
```
