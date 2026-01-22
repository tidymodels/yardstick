# Recall

These functions calculate the `recall()` of a measurement system for
finding relevant documents compared to reference results (the truth
regarding relevance). Highly related functions are
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md)
and
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md).

## Usage

``` r
recall(data, ...)

# S3 method for class 'data.frame'
recall(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

recall_vec(
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

For `recall_vec()`, a single `numeric` value (or `NA`).

## Details

The recall (aka sensitivity) is defined as the proportion of relevant
results out of the number of samples which were actually relevant. When
there are no relevant results, recall is not defined and a value of `NA`
is returned.

When the denominator of the calculation is `0`, recall is undefined.
This happens when both `# true_positive = 0` and `# false_negative = 0`
are true, which mean that there were no true events. When computing
binary recall, a `NA` value will be returned with a warning. When
computing multiclass recall, the individual `NA` values will be removed,
and the computation will procede, with a warning.

Suppose a 2x2 table with notation:

|            |           |            |
|------------|-----------|------------|
|            | Reference |            |
| Predicted  | Relevant  | Irrelevant |
| Relevant   | A         | B          |
| Irrelevant | C         | D          |

The formula used here is:

\$\$\text{Recall} = \frac{A}{A + C}\$\$

Recall is a metric that should be maximized. The output ranges from 0 to
1, with 1 indicating that all actual positives were predicted as
positive.

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
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

Other relevance metrics:
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md)

## Author

Max Kuhn

## Examples

``` r
# Two class
data("two_class_example")
recall(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 recall  binary         0.880

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  recall(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 recall  macro          0.548

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  recall(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   recall  macro          0.548
#>  2 Fold02   recall  macro          0.541
#>  3 Fold03   recall  macro          0.634
#>  4 Fold04   recall  macro          0.570
#>  5 Fold05   recall  macro          0.550
#>  6 Fold06   recall  macro          0.540
#>  7 Fold07   recall  macro          0.531
#>  8 Fold08   recall  macro          0.584
#>  9 Fold09   recall  macro          0.568
#> 10 Fold10   recall  macro          0.537

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  recall(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   recall  macro_weighted     0.726
#>  2 Fold02   recall  macro_weighted     0.712
#>  3 Fold03   recall  macro_weighted     0.758
#>  4 Fold04   recall  macro_weighted     0.712
#>  5 Fold05   recall  macro_weighted     0.712
#>  6 Fold06   recall  macro_weighted     0.697
#>  7 Fold07   recall  macro_weighted     0.675
#>  8 Fold08   recall  macro_weighted     0.721
#>  9 Fold09   recall  macro_weighted     0.673
#> 10 Fold10   recall  macro_weighted     0.699

# Vector version
recall_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.879845

# Making Class2 the "relevant" level
recall_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.7933884
```
