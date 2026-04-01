# Symmetric Extremal Dependence Index

Symmetric Extremal Dependence Index (SEDI) is a skill metric for
classification that remains reliable at extreme prevalence levels where
traditional metrics (TSS, MCC, Kappa) degrade. It is defined using the
hit rate (sensitivity) and false alarm rate (1 - specificity):

\$\$\text{SEDI} = \frac{\ln F - \ln H - \ln(1-F) + \ln(1-H)} {\ln F +
\ln H + \ln(1-F) + \ln(1-H)}\$\$

where \\H\\ is sensitivity (hit rate) and \\F\\ is the false alarm rate
(1 - specificity).

## Usage

``` r
sedi(data, ...)

# S3 method for class 'data.frame'
sedi(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

sedi_vec(
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

For `sedi_vec()`, a single `numeric` value (or `NA`).

## Details

Suppose a 2x2 table with notation:

|           |           |          |
|-----------|-----------|----------|
|           | Reference |          |
| Predicted | Positive  | Negative |
| Positive  | A         | B        |
| Negative  | C         | D        |

The formulas used here are:

\$\$H = \text{Sensitivity} = \frac{A}{A + C}\$\$

\$\$F = 1 - \text{Specificity} = \frac{B}{B + D}\$\$

SEDI is a metric that should be maximized. The output ranges from -1 to
1, with 1 indicating perfect discrimination.

SEDI is **base-rate independent**: its value depends only on sensitivity
and specificity (class-conditional rates), not on prevalence. The
logarithmic transformation ensures the metric remains discriminating
even when events are extremely rare (prevalence \< 2.5%), where
[`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md)
(TSS) converges to the hit rate alone and
[`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md)
exhibits denominator suppression.

When sensitivity or specificity is exactly 0 or 1, the logarithm is
undefined. A small constant (`1e-9`) is used to clamp values away from
these boundaries.

## Prevalence guidance

- **Prevalence \>= 10%**: MCC, TSS, and SEDI all perform well.

- **Prevalence 2.5-10%**: SEDI preferred; MCC and TSS still usable.

- **Prevalence \< 2.5%**: SEDI strongly recommended; MCC and TSS
  unreliable.

## Multiclass

Macro, micro, and macro-weighted averaging is available for this metric.
The default is to select macro averaging if a `truth` factor with more
than 2 levels is provided. Otherwise, a standard binary calculation is
done. See
[`vignette("multiclass", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/multiclass.md)
for more information.

For multiclass problems, SEDI is computed via one-vs-all decomposition:
each class is treated as a binary problem against all other classes, and
a per-class SEDI is calculated. Macro averaging (the default) weights
all classes equally, which is recommended since SEDI's log transform
already handles class imbalance internally. Macro-weighted averaging
weights by class prevalence. Micro averaging pools counts across classes
before computing a single SEDI value.

## Relevant Level

There is no common convention on which factor level should automatically
be considered the "event" or "positive" result when computing binary
classification metrics. In `yardstick`, the default is to use the
*first* level. To alter this, change the argument `event_level` to
`"second"` to consider the *last* level of the factor the level of
interest. For multiclass extensions involving one-vs-all comparisons
(such as macro averaging), this option is ignored and the "one" level is
always the relevant result.

## References

Ferro, C.A.T. and Stephenson, D.B. (2011). "Extremal Dependence Indices:
Improved Verification Measures for Deterministic Forecasts of Rare
Binary Events". Weather and Forecasting. 26 (5): 699-713.

Wunderlich, R.F., Lin, Y.-P., Anthony, J. and Petway, J.R. (2019). "Two
alternative evaluation metrics to replace the true skill statistic in
the assessment of species distribution models". Nature Conservation. 35:
97-116.

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

Simon Dedman

## Examples

``` r
# Two class
data("two_class_example")
sedi(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 sedi    binary         0.823

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  sedi(obs, pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 sedi    macro          0.633

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  sedi(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   sedi    macro          0.633
#>  2 Fold02   sedi    macro          0.620
#>  3 Fold03   sedi    macro          0.735
#>  4 Fold04   sedi    macro          0.647
#>  5 Fold05   sedi    macro          0.645
#>  6 Fold06   sedi    macro          0.621
#>  7 Fold07   sedi    macro          0.580
#>  8 Fold08   sedi    macro          0.667
#>  9 Fold09   sedi    macro          0.615
#> 10 Fold10   sedi    macro          0.621

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  sedi(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   sedi    macro_weighted     0.713
#>  2 Fold02   sedi    macro_weighted     0.699
#>  3 Fold03   sedi    macro_weighted     0.773
#>  4 Fold04   sedi    macro_weighted     0.688
#>  5 Fold05   sedi    macro_weighted     0.702
#>  6 Fold06   sedi    macro_weighted     0.671
#>  7 Fold07   sedi    macro_weighted     0.633
#>  8 Fold08   sedi    macro_weighted     0.710
#>  9 Fold09   sedi    macro_weighted     0.630
#> 10 Fold10   sedi    macro_weighted     0.676

# Vector version
sedi_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.8227266

# Making Class2 the "relevant" level
sedi_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.8227266
```
