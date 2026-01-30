# J-index

Youden's J statistic is defined as:

[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) +
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md) - 1

A related metric is Informedness, see the Details section for the
relationship.

## Usage

``` r
j_index(data, ...)

# S3 method for class 'data.frame'
j_index(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

j_index_vec(
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

For `j_index_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formulas used here are:

\$\$\text{Sensitivity} = \frac{A}{A + C}\$\$

\$\$\text{Specificity} = \frac{D}{B + D}\$\$

\$\$\text{J-index} = \text{Sensitivity} + \text{Specificity} - 1\$\$

J-index is a metric that should be maximized. The output ranges from -1
to 1, with 1 indicating no false positives and no false negatives.

The binary version of J-index is equivalent to the binary concept of
Informedness. Macro-weighted J-index is equivalent to multiclass
informedness as defined in Powers, David M W (2011), equation (42).

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

Youden, W.J. (1950). "Index for rating diagnostic tests". Cancer. 3:
32-35.

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
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
[`markedness()`](https://yardstick.tidymodels.org/dev/reference/markedness.md),
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
# Two class
data("two_class_example")
j_index(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 j_index binary         0.673

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  j_index(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 j_index macro          0.434

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  j_index(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   j_index macro          0.434
#>  2 Fold02   j_index macro          0.422
#>  3 Fold03   j_index macro          0.533
#>  4 Fold04   j_index macro          0.449
#>  5 Fold05   j_index macro          0.431
#>  6 Fold06   j_index macro          0.413
#>  7 Fold07   j_index macro          0.398
#>  8 Fold08   j_index macro          0.468
#>  9 Fold09   j_index macro          0.435
#> 10 Fold10   j_index macro          0.412

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  j_index(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   j_index macro_weighted     0.542
#>  2 Fold02   j_index macro_weighted     0.527
#>  3 Fold03   j_index macro_weighted     0.597
#>  4 Fold04   j_index macro_weighted     0.515
#>  5 Fold05   j_index macro_weighted     0.524
#>  6 Fold06   j_index macro_weighted     0.492
#>  7 Fold07   j_index macro_weighted     0.466
#>  8 Fold08   j_index macro_weighted     0.535
#>  9 Fold09   j_index macro_weighted     0.468
#> 10 Fold10   j_index macro_weighted     0.501

# Vector version
j_index_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.6732334

# Making Class2 the "relevant" level
j_index_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.6732334
```
