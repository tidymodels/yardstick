# Miss rate (False Negative Rate)

These functions calculate the miss rate (false negative rate) of a
measurement system compared to a reference result (the "truth" or gold
standard). Miss rate is defined as `1 - sensitivity`, or equivalently,
the proportion of positives that are incorrectly classified as
negatives.

## Usage

``` r
miss_rate(data, ...)

# S3 method for class 'data.frame'
miss_rate(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
)

miss_rate_vec(
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

For `miss_rate_vec()`, a single `numeric` value (or `NA`).

## Details

Miss rate is also known as the false negative rate (FNR) or the
probability of miss.

When the denominator of the calculation is `0`, miss rate is undefined.
This happens when both `# true_positive = 0` and `# false_negative = 0`
are true, which means that there were no events. When computing binary
miss rate, a `NA` value will be returned with a warning. When computing
multiclass miss rate, the individual `NA` values will be removed, and
the computation will proceed, with a warning.

Miss rate is a metric that should be minimized. The output ranges from 0
to 1, with 0 indicating that all actual positives were correctly
predicted as positive (no false negatives).

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

\$\$\text{Sensitivity} = \frac{A}{A + C}\$\$

\$\$\text{Specificity} = \frac{D}{B + D}\$\$

\$\$\text{Prevalence} = \frac{A + C}{A + B + C + D}\$\$

\$\$\text{PPV} = \frac{\text{Sensitivity} \cdot
\text{Prevalence}}{(\text{Sensitivity} \cdot \text{Prevalence}) + ((1 -
\text{Specificity}) \cdot (1 - \text{Prevalence}))}\$\$

\$\$\text{NPV} = \frac{\text{Specificity} \cdot (1 -
\text{Prevalence})}{((1 - \text{Sensitivity}) \cdot \text{Prevalence}) +
((\text{Specificity}) \cdot (1-Prevalence))}\$\$

See the references for discussions of the statistics.

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
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

Other sensitivity metrics:
[`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md),
[`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
[`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)

## Examples

``` r
# Two class
data("two_class_example")
miss_rate(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 miss_rate binary         0.120

# Multiclass
library(dplyr)
data(hpc_cv)

hpc_cv |>
  filter(Resample == "Fold01") |>
  miss_rate(obs, pred)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 miss_rate macro          0.452

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  miss_rate(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric   .estimator .estimate
#>    <chr>    <chr>     <chr>          <dbl>
#>  1 Fold01   miss_rate macro          0.452
#>  2 Fold02   miss_rate macro          0.459
#>  3 Fold03   miss_rate macro          0.366
#>  4 Fold04   miss_rate macro          0.430
#>  5 Fold05   miss_rate macro          0.450
#>  6 Fold06   miss_rate macro          0.460
#>  7 Fold07   miss_rate macro          0.469
#>  8 Fold08   miss_rate macro          0.416
#>  9 Fold09   miss_rate macro          0.432
#> 10 Fold10   miss_rate macro          0.463

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  miss_rate(obs, pred, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric   .estimator     .estimate
#>    <chr>    <chr>     <chr>              <dbl>
#>  1 Fold01   miss_rate macro_weighted     0.274
#>  2 Fold02   miss_rate macro_weighted     0.288
#>  3 Fold03   miss_rate macro_weighted     0.242
#>  4 Fold04   miss_rate macro_weighted     0.288
#>  5 Fold05   miss_rate macro_weighted     0.288
#>  6 Fold06   miss_rate macro_weighted     0.303
#>  7 Fold07   miss_rate macro_weighted     0.325
#>  8 Fold08   miss_rate macro_weighted     0.279
#>  9 Fold09   miss_rate macro_weighted     0.327
#> 10 Fold10   miss_rate macro_weighted     0.301

# Vector version
miss_rate_vec(
  two_class_example$truth,
  two_class_example$predicted
)
#> [1] 0.120155

# Making Class2 the "relevant" level
miss_rate_vec(
  two_class_example$truth,
  two_class_example$predicted,
  event_level = "second"
)
#> [1] 0.2066116
```
