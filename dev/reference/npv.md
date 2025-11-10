# Negative predictive value

These functions calculate the `npv()` (negative predictive value) of a
measurement system compared to a reference result (the "truth" or gold
standard). Highly related functions are
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md), and
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md).

## Usage

``` r
npv(data, ...)

# S3 method for class 'data.frame'
npv(
  data,
  truth,
  estimate,
  prevalence = NULL,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

npv_vec(
  truth,
  estimate,
  prevalence = NULL,
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

- prevalence:

  A numeric value for the rate of the "positive" class of the data.

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

For `npv_vec()`, a single `numeric` value (or `NA`).

## Details

The positive predictive value
([`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md)) is
defined as the percent of predicted positives that are actually positive
while the negative predictive value (`npv()`) is defined as the percent
of negative positives that are actually negative.

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

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formulas used here are:

\$\$Sensitivity = A/(A+C)\$\$ \$\$Specificity = D/(B+D)\$\$
\$\$Prevalence = (A+C)/(A+B+C+D)\$\$ \$\$PPV = (Sensitivity \*
Prevalence) / ((Sensitivity \* Prevalence) + ((1-Specificity) \*
(1-Prevalence)))\$\$ \$\$NPV = (Specificity \* (1-Prevalence)) /
(((1-Sensitivity) \* Prevalence) + ((Specificity) \*
(1-Prevalence)))\$\$

See the references for discussions of the statistics.

## References

Altman, D.G., Bland, J.M. (1994) “Diagnostic tests 2: predictive
values,” *British Medical Journal*, vol 309, 102.

## See also

Other class metrics:
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
[`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
[`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md),
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md),
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
[`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md),
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

Other sensitivity metrics:
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Author

Max Kuhn

## Examples

``` r
# Two class
data("two_class_example")
npv(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 npv     binary         0.861

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  npv(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 npv     macro          0.906

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  npv(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   npv     macro          0.906
#>  2 Fold02   npv     macro          0.901
#>  3 Fold03   npv     macro          0.917
#>  4 Fold04   npv     macro          0.897
#>  5 Fold05   npv     macro          0.897
#>  6 Fold06   npv     macro          0.892
#>  7 Fold07   npv     macro          0.882
#>  8 Fold08   npv     macro          0.902
#>  9 Fold09   npv     macro          0.879
#> 10 Fold10   npv     macro          0.890

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  npv(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   npv     macro_weighted     0.896
#>  2 Fold02   npv     macro_weighted     0.890
#>  3 Fold03   npv     macro_weighted     0.905
#>  4 Fold04   npv     macro_weighted     0.878
#>  5 Fold05   npv     macro_weighted     0.878
#>  6 Fold06   npv     macro_weighted     0.871
#>  7 Fold07   npv     macro_weighted     0.853
#>  8 Fold08   npv     macro_weighted     0.885
#>  9 Fold09   npv     macro_weighted     0.845
#> 10 Fold10   npv     macro_weighted     0.864

# Vector version
npv_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.8609865

# Making Class2 the "relevant" level
npv_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.8194946
```
