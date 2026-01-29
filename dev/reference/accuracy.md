# Accuracy

Accuracy is the proportion of the data that are predicted correctly.

## Usage

``` r
accuracy(data, ...)

# S3 method for class 'data.frame'
accuracy(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

accuracy_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `accuracy_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formula used here is:

\$\$\text{Accuracy} = \frac{A + D}{A + B + C + D}\$\$

Accuracy is a metric that should be maximized. The output ranges from 0
to 1, with 1 indicating perfect predictions.

## Multiclass

Accuracy extends naturally to multiclass scenarios. Because of this,
macro and micro averaging are not implemented.

## See also

[All class
metrics](https://yardstick.tidymodels.org/dev/reference/class-metrics.md)

Other class metrics:
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`roc_dist()`](https://yardstick.tidymodels.org/dev/reference/roc_dist.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Author

Max Kuhn

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data("two_class_example")
data("hpc_cv")

# Two class
accuracy(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.838

# Multiclass
# accuracy() has a natural multiclass extension
hpc_cv |>
  filter(Resample == "Fold01") |>
  accuracy(obs, pred)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy multiclass     0.726

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  accuracy(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
```
