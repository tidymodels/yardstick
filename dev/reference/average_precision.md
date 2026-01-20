# Area under the precision recall curve

`average_precision()` is an alternative to
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md)
that avoids any ambiguity about what the value of `precision` should be
when `recall == 0` and there are not yet any false positive values (some
say it should be `0`, others say `1`, others say undefined).

It computes a weighted average of the precision values returned from
[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md),
where the weights are the increase in recall from the previous
threshold. See
[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
for the full curve.

## Usage

``` r
average_precision(data, ...)

# S3 method for class 'data.frame'
average_precision(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
)

average_precision_vec(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth` and `...`.

- ...:

  A set of unquoted column names or one or more `dplyr` selector
  functions to choose which variables contain the class probabilities.
  If `truth` is binary, only 1 column should be selected, and it should
  correspond to the value of `event_level`. Otherwise, there should be
  as many columns as factor levels of `truth` and the ordering of the
  columns should be the same as the factor levels of `truth`.

- truth:

  The column identifier for the true class results (that is a `factor`).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `factor`
  vector.

- estimator:

  One of `"binary"`, `"macro"`, or `"macro_weighted"` to specify the
  type of averaging to be done. `"binary"` is only relevant for the two
  class case. The other two are general methods for calculating
  multiclass metrics. The default will automatically choose `"binary"`
  or `"macro"` based on `truth`.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event". This argument is only
  applicable when `estimator = "binary"`. The default uses an internal
  helper that defaults to `"first"`.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in `data`. For
  `_vec()` functions, a numeric vector,
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
  or
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

- estimate:

  If `truth` is binary, a numeric vector of class probabilities
  corresponding to the "relevant" class. Otherwise, a matrix with as
  many columns as factor levels of `truth`. *It is assumed that these
  are in the same order as the levels of `truth`.*

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `average_precision_vec()`, a single `numeric` value (or `NA`).

## Details

Average precision is a metric that should be maximized. The output
ranges from 0 to 1, with 1 indicating perfect precision and recall at
all thresholds.

The computation for average precision is a weighted average of the
precision values. Assuming you have `n` rows returned from
[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md),
it is a sum from `2` to `n`, multiplying the precision value `p_i` by
the increase in recall over the previous threshold, `r_i - r_(i-1)`.

\$\$AP = \sum (r\_{i} - r\_{i-1}) \cdot p_i\$\$

By summing from `2` to `n`, the precision value `p_1` is never used.
While
[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
returns a value for `p_1`, it is technically undefined as
`tp / (tp + fp)` with `tp = 0` and `fp = 0`. A common convention is to
use `1` for `p_1`, but this metric has the nice property of avoiding the
ambiguity. On the other hand, `r_1` is well defined as long as there are
some events (`p`), and it is `tp / p` with `tp = 0`, so `r_1 = 0`.

When `p_1` is defined as `1`, the `average_precision()` and
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)
values are often very close to one another.

## Multiclass

Macro and macro-weighted averaging is available for this metric. The
default is to select macro averaging if a `truth` factor with more than
2 levels is provided. Otherwise, a standard binary calculation is done.
See
[`vignette("multiclass", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/multiclass.md)
for more information.

## Relevant Level

There is no common convention on which factor level should automatically
be considered the "event" or "positive" result when computing binary
classification metrics. In `yardstick`, the default is to use the
*first* level. To alter this, change the argument `event_level` to
`"second"` to consider the *last* level of the factor the level of
interest. For multiclass extensions involving one-vs-all comparisons
(such as macro averaging), this option is ignored and the "one" level is
always the relevant result.

## See also

[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
for computing the full precision recall curve.

[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md)
for computing the area under the precision recall curve using the
trapezoidal rule.

Other class probability metrics:
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Examples

``` r
# ---------------------------------------------------------------------------
# Two class example

# `truth` is a 2 level factor. The first level is `"Class1"`, which is the
# "event of interest" by default in yardstick. See the Relevant Level
# section above.
data(two_class_example)

# Binary metrics using class probabilities take a factor `truth` column,
# and a single class probability column containing the probabilities of
# the event of interest. Here, since `"Class1"` is the first level of
# `"truth"`, it is the event of interest and we pass in probabilities for it.
average_precision(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric           .estimator .estimate
#>   <chr>             <chr>          <dbl>
#> 1 average_precision binary         0.947

# ---------------------------------------------------------------------------
# Multiclass example

# `obs` is a 4 level factor. The first level is `"VF"`, which is the
# "event of interest" by default in yardstick. See the Relevant Level
# section above.
data(hpc_cv)

# You can use the col1:colN tidyselect syntax
library(dplyr)
hpc_cv |>
  filter(Resample == "Fold01") |>
  average_precision(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric           .estimator .estimate
#>   <chr>             <chr>          <dbl>
#> 1 average_precision macro          0.617

# Change the first level of `obs` from `"VF"` to `"M"` to alter the
# event of interest. The class probability columns should be supplied
# in the same order as the levels.
hpc_cv |>
  filter(Resample == "Fold01") |>
  mutate(obs = relevel(obs, "M")) |>
  average_precision(obs, M, VF:L)
#> # A tibble: 1 × 3
#>   .metric           .estimator .estimate
#>   <chr>             <chr>          <dbl>
#> 1 average_precision macro          0.617

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  average_precision(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric           .estimator .estimate
#>    <chr>    <chr>             <chr>          <dbl>
#>  1 Fold01   average_precision macro          0.617
#>  2 Fold02   average_precision macro          0.625
#>  3 Fold03   average_precision macro          0.699
#>  4 Fold04   average_precision macro          0.685
#>  5 Fold05   average_precision macro          0.625
#>  6 Fold06   average_precision macro          0.656
#>  7 Fold07   average_precision macro          0.617
#>  8 Fold08   average_precision macro          0.659
#>  9 Fold09   average_precision macro          0.632
#> 10 Fold10   average_precision macro          0.611

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  average_precision(obs, VF:L, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric           .estimator     .estimate
#>    <chr>    <chr>             <chr>              <dbl>
#>  1 Fold01   average_precision macro_weighted     0.750
#>  2 Fold02   average_precision macro_weighted     0.745
#>  3 Fold03   average_precision macro_weighted     0.794
#>  4 Fold04   average_precision macro_weighted     0.757
#>  5 Fold05   average_precision macro_weighted     0.740
#>  6 Fold06   average_precision macro_weighted     0.747
#>  7 Fold07   average_precision macro_weighted     0.751
#>  8 Fold08   average_precision macro_weighted     0.759
#>  9 Fold09   average_precision macro_weighted     0.714
#> 10 Fold10   average_precision macro_weighted     0.742

# Vector version
# Supply a matrix of class probabilities
fold1 <- hpc_cv |>
  filter(Resample == "Fold01")

average_precision_vec(
   truth = fold1$obs,
   matrix(
     c(fold1$VF, fold1$F, fold1$M, fold1$L),
     ncol = 4
   )
)
#> [1] 0.6173363
```
