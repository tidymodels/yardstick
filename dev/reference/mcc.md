# Matthews correlation coefficient

Matthews correlation coefficient

## Usage

``` r
mcc(data, ...)

# S3 method for class 'data.frame'
mcc(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

mcc_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `mcc_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formula used here is:

\$\$\text{MCC} = \frac{(A \cdot D) - (B \cdot C)}{\sqrt{(A + B)(A +
C)(D + B)(D + C)}}\$\$

MCC is a metric that should be maximized. The output ranges from -1 to
1, with 1 indicating perfect predictions. A value of 0 indicates no
better than random prediction, and negative values indicate inverse
prediction.

## Relevant Level

There is no common convention on which factor level should automatically
be considered the "event" or "positive" result when computing binary
classification metrics. In `yardstick`, the default is to use the
*first* level. To alter this, change the argument `event_level` to
`"second"` to consider the *last* level of the factor the level of
interest. For multiclass extensions involving one-vs-all comparisons
(such as macro averaging), this option is ignored and the "one" level is
always the relevant result.

## Multiclass

`mcc()` has a known multiclass generalization and that is computed
automatically if a factor with more than 2 levels is provided. Because
of this, no averaging methods are provided.

## References

Giuseppe, J. (2012). "A Comparison of MCC and CEN Error Measures in
Multi-Class Prediction". *PLOS ONE*. Vol 7, Iss 8, e41882.

## See also

[All class
metrics](https://yardstick.tidymodels.org/dev/reference/class-metrics.md)

Other class metrics:
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
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
data("two_class_example")
data("hpc_cv")

# Two class
mcc(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mcc     binary         0.677

# Multiclass
# mcc() has a natural multiclass extension
hpc_cv |>
  filter(Resample == "Fold01") |>
  mcc(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mcc     multiclass     0.542

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  mcc(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   mcc     multiclass     0.542
#>  2 Fold02   mcc     multiclass     0.521
#>  3 Fold03   mcc     multiclass     0.602
#>  4 Fold04   mcc     multiclass     0.519
#>  5 Fold05   mcc     multiclass     0.520
#>  6 Fold06   mcc     multiclass     0.494
#>  7 Fold07   mcc     multiclass     0.461
#>  8 Fold08   mcc     multiclass     0.538
#>  9 Fold09   mcc     multiclass     0.459
#> 10 Fold10   mcc     multiclass     0.498
```
