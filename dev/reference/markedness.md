# Markedness

Markedness is defined as:

[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md) +
"inverse precision" - 1

where "inverse precision" is the proportion of true negatives among all
predicted negatives. A related metric is Informedness, see the Details
section for the relationship.

## Usage

``` r
markedness(data, ...)

# S3 method for class 'data.frame'
markedness(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

markedness_vec(
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

For `markedness_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formulas used here are:

\$\$\text{Precision} = \frac{A}{A + B}\$\$

\$\$\text{Inverse Precision} = \frac{D}{C + D}\$\$

\$\$\text{Markedness} = \text{Precision} + \text{Inverse Precision} -
1\$\$

Markedness is a metric that should be maximized. The output ranges from
-1 to 1, with 1 indicating perfect predictions.

Markedness is to the predicted condition (precision and inverse
precision) what Informedness
([`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md))
is to the actual condition (sensitivity and specificity).

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

## References

Powers, David M W (2011). "Evaluation: From Precision, Recall and
F-Score to ROC, Informedness, Markedness and Correlation". Journal of
Machine Learning Technologies. 2 (1): 37-63.

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
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`roc_dist()`](https://yardstick.tidymodels.org/dev/reference/roc_dist.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Examples

``` r
# Two class
data("two_class_example")
markedness(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric    .estimator .estimate
#>   <chr>      <chr>          <dbl>
#> 1 markedness binary         0.680

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  markedness(obs, pred)
#> # A tibble: 1 × 3
#>   .metric    .estimator .estimate
#>   <chr>      <chr>          <dbl>
#> 1 markedness macro          0.543

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  markedness(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric    .estimator .estimate
#>    <chr>    <chr>      <chr>          <dbl>
#>  1 Fold01   markedness macro          0.543
#>  2 Fold02   markedness macro          0.504
#>  3 Fold03   markedness macro          0.622
#>  4 Fold04   markedness macro          0.556
#>  5 Fold05   markedness macro          0.548
#>  6 Fold06   markedness macro          0.518
#>  7 Fold07   markedness macro          0.444
#>  8 Fold08   markedness macro          0.554
#>  9 Fold09   markedness macro          0.484
#> 10 Fold10   markedness macro          0.515

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  markedness(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric    .estimator     .estimate
#>    <chr>    <chr>      <chr>              <dbl>
#>  1 Fold01   markedness macro_weighted     0.592
#>  2 Fold02   markedness macro_weighted     0.579
#>  3 Fold03   markedness macro_weighted     0.657
#>  4 Fold04   markedness macro_weighted     0.568
#>  5 Fold05   markedness macro_weighted     0.583
#>  6 Fold06   markedness macro_weighted     0.553
#>  7 Fold07   markedness macro_weighted     0.502
#>  8 Fold08   markedness macro_weighted     0.587
#>  9 Fold09   markedness macro_weighted     0.506
#> 10 Fold10   markedness macro_weighted     0.547

# Vector version
markedness_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.6804811

# Making Class2 the "relevant" level
markedness_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.6804811
```
