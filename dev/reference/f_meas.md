# F Measure

These functions calculate the `f_meas()` of a measurement system for
finding relevant documents compared to reference results (the truth
regarding relevance). Highly related functions are
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md)
and
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md).

## Usage

``` r
f_meas(data, ...)

# S3 method for class 'data.frame'
f_meas(
  data,
  truth,
  estimate,
  beta = 1,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

f_meas_vec(
  truth,
  estimate,
  beta = 1,
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

- beta:

  A numeric value used to weight precision and recall. A value of 1 is
  traditionally used and corresponds to the harmonic mean of the two
  values but other values weight recall beta times more important than
  precision.

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

For `f_meas_vec()`, a single `numeric` value (or `NA`).

## Details

The measure "F" is a combination of precision and recall (see below).

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

## Implementation

Suppose a 2x2 table with notation:

|            |           |            |
|------------|-----------|------------|
|            | Reference |            |
| Predicted  | Relevant  | Irrelevant |
| Relevant   | A         | B          |
| Irrelevant | C         | D          |

The formulas used here are:

\$\$\text{recall} = \frac{A}{A + C}\$\$ \$\$\text{precision} =
\frac{A}{A + B}\$\$ \$\$F\_{meas} = \frac{(1 + \beta^2) \cdot
\text{precision} \cdot \text{recall}}{\beta^2 \cdot \text{precision} +
\text{recall}}\$\$

See the references for discussions of the statistics.

## References

Buckland, M., & Gey, F. (1994). The relationship between Recall and
Precision. *Journal of the American Society for Information Science*,
45(1), 12-19.

Powers, D. (2007). Evaluation: From Precision, Recall and F Factor to
ROC, Informedness, Markedness and Correlation. Technical Report
SIE-07-001, Flinders University

## See also

Other class metrics:
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

Other relevance metrics:
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md)

## Author

Max Kuhn

## Examples

``` r
# Two class
data("two_class_example")
f_meas(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 f_meas  binary         0.849

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  f_meas(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 f_meas  macro          0.563

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  f_meas(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   f_meas  macro          0.563
#>  2 Fold02   f_meas  macro          0.542
#>  3 Fold03   f_meas  macro          0.641
#>  4 Fold04   f_meas  macro          0.593
#>  5 Fold05   f_meas  macro          0.570
#>  6 Fold06   f_meas  macro          0.554
#>  7 Fold07   f_meas  macro          0.516
#>  8 Fold08   f_meas  macro          0.601
#>  9 Fold09   f_meas  macro          0.555
#> 10 Fold10   f_meas  macro          0.560

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  f_meas(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   f_meas  macro_weighted     0.696
#>  2 Fold02   f_meas  macro_weighted     0.684
#>  3 Fold03   f_meas  macro_weighted     0.739
#>  4 Fold04   f_meas  macro_weighted     0.689
#>  5 Fold05   f_meas  macro_weighted     0.692
#>  6 Fold06   f_meas  macro_weighted     0.673
#>  7 Fold07   f_meas  macro_weighted     0.646
#>  8 Fold08   f_meas  macro_weighted     0.701
#>  9 Fold09   f_meas  macro_weighted     0.652
#> 10 Fold10   f_meas  macro_weighted     0.680

# Vector version
f_meas_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.8485981

# Making Class2 the "relevant" level
f_meas_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.8258065
```
