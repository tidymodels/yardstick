# Detection prevalence

Detection prevalence is defined as the number of *predicted* positive
events (both true positive and false positive) divided by the total
number of predictions.

## Usage

``` r
detection_prevalence(data, ...)

# S3 method for class 'data.frame'
detection_prevalence(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

detection_prevalence_vec(
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

For `detection_prevalence_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formula used here is:

\$\$\text{Detection Prevalence} = \frac{A + B}{A + B + C + D}\$\$

Detection prevalence is a metric that should be maximized. The output
ranges from 0 to 1. The "optimal" value depends on the true prevalence
of positive events in the data.

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
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
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
detection_prevalence(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric              .estimator .estimate
#>   <chr>                <chr>          <dbl>
#> 1 detection_prevalence binary         0.554

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  detection_prevalence(obs, pred)
#> # A tibble: 1 × 3
#>   .metric              .estimator .estimate
#>   <chr>                <chr>          <dbl>
#> 1 detection_prevalence macro           0.25

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  detection_prevalence(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric              .estimator .estimate
#>    <chr>    <chr>                <chr>          <dbl>
#>  1 Fold01   detection_prevalence macro           0.25
#>  2 Fold02   detection_prevalence macro           0.25
#>  3 Fold03   detection_prevalence macro           0.25
#>  4 Fold04   detection_prevalence macro           0.25
#>  5 Fold05   detection_prevalence macro           0.25
#>  6 Fold06   detection_prevalence macro           0.25
#>  7 Fold07   detection_prevalence macro           0.25
#>  8 Fold08   detection_prevalence macro           0.25
#>  9 Fold09   detection_prevalence macro           0.25
#> 10 Fold10   detection_prevalence macro           0.25

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  detection_prevalence(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric              .estimator     .estimate
#>    <chr>    <chr>                <chr>              <dbl>
#>  1 Fold01   detection_prevalence macro_weighted     0.413
#>  2 Fold02   detection_prevalence macro_weighted     0.409
#>  3 Fold03   detection_prevalence macro_weighted     0.404
#>  4 Fold04   detection_prevalence macro_weighted     0.411
#>  5 Fold05   detection_prevalence macro_weighted     0.407
#>  6 Fold06   detection_prevalence macro_weighted     0.411
#>  7 Fold07   detection_prevalence macro_weighted     0.405
#>  8 Fold08   detection_prevalence macro_weighted     0.406
#>  9 Fold09   detection_prevalence macro_weighted     0.402
#> 10 Fold10   detection_prevalence macro_weighted     0.408

# Vector version
detection_prevalence_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.554

# Making Class2 the "relevant" level
detection_prevalence_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.446
```
