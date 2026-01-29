# Fall-out (False Positive Rate)

These functions calculate the fall-out (false positive rate) of a
measurement system compared to a reference result (the "truth" or gold
standard). Fall-out is defined as `1 - specificity`, or equivalently,
the proportion of negatives that are incorrectly classified as
positives.

## Usage

``` r
fall_out(data, ...)

# S3 method for class 'data.frame'
fall_out(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

fall_out_vec(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
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

- estimator:

  One of: `"binary"`, `"macro"`, `"macro_weighted"`, or `"micro"` to
  specify the type of averaging to be done. `"binary"` is only relevant
  for the two class case. The other three are general methods for
  calculating multiclass metrics. The default will automatically choose
  `"binary"` or `"macro"` based on `estimate`.

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

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event". This argument is only
  applicable when `estimator = "binary"`. The default uses an internal
  helper that defaults to `"first"`.

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `fall_out_vec()`, a single `numeric` value (or `NA`).

## Details

Fall-out is also known as the false positive rate (FPR) or the
probability of false alarm.

When the denominator of the calculation is `0`, fall-out is undefined.
This happens when both `# true_negative = 0` and `# false_positive = 0`
are true, which means that there were no negatives. When computing
binary fall-out, a `NA` value will be returned with a warning. When
computing multiclass fall-out, the individual `NA` values will be
removed, and the computation will proceed, with a warning.

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formula used here is:

\$\$\text{Fall-out} = \frac{B}{B + D}\$\$

Fall-out is a metric that should be minimized. The output ranges from 0
to 1, with 0 indicating that all actual negatives were correctly
predicted as negative (no false positives).

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

Macro, micro, and macro-weighted averaging is available for this metric.
The default is to select macro averaging if a `truth` factor with more
than 2 levels is provided. Otherwise, a standard binary calculation is
done. See
[`vignette("multiclass", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/multiclass.md)
for more information.

## See also

[All class
metrics](https://yardstick.tidymodels.org/dev/reference/class-metrics.md)

Other class metrics:
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
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

Other sensitivity metrics:
[`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Examples

``` r
# Two class
data("two_class_example")
fall_out(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 fall_out binary         0.207

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  fall_out(obs, pred)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 fall_out macro          0.114

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  fall_out(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   fall_out macro          0.114
#>  2 Fold02   fall_out macro          0.118
#>  3 Fold03   fall_out macro          0.101
#>  4 Fold04   fall_out macro          0.121
#>  5 Fold05   fall_out macro          0.119
#>  6 Fold06   fall_out macro          0.127
#>  7 Fold07   fall_out macro          0.134
#>  8 Fold08   fall_out macro          0.116
#>  9 Fold09   fall_out macro          0.133
#> 10 Fold10   fall_out macro          0.125

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  fall_out(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator     .estimate
#>    <chr>    <chr>    <chr>              <dbl>
#>  1 Fold01   fall_out macro_weighted     0.184
#>  2 Fold02   fall_out macro_weighted     0.185
#>  3 Fold03   fall_out macro_weighted     0.161
#>  4 Fold04   fall_out macro_weighted     0.197
#>  5 Fold05   fall_out macro_weighted     0.188
#>  6 Fold06   fall_out macro_weighted     0.205
#>  7 Fold07   fall_out macro_weighted     0.210
#>  8 Fold08   fall_out macro_weighted     0.186
#>  9 Fold09   fall_out macro_weighted     0.205
#> 10 Fold10   fall_out macro_weighted     0.199

# Vector version
fall_out_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.2066116

# Making Class2 the "relevant" level
fall_out_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.120155
```
