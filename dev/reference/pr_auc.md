# Area under the precision recall curve

`pr_auc()` is a metric that computes the area under the precision recall
curve. See
[`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
for the full curve.

## Usage

``` r
pr_auc(data, ...)

# S3 method for class 'data.frame'
pr_auc(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
)

pr_auc_vec(
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

For `pr_auc_vec()`, a single `numeric` value (or `NA`).

## Details

PR AUC is a metric that should be maximized. The output ranges from 0 to
1, with 1 indicating perfect precision and recall at all thresholds.

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

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Author

Max Kuhn

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
pr_auc(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 pr_auc  binary         0.946

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
  pr_auc(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 pr_auc  macro          0.611

# Change the first level of `obs` from `"VF"` to `"M"` to alter the
# event of interest. The class probability columns should be supplied
# in the same order as the levels.
hpc_cv |>
  filter(Resample == "Fold01") |>
  mutate(obs = relevel(obs, "M")) |>
  pr_auc(obs, M, VF:L)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 pr_auc  macro          0.611

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  pr_auc(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   pr_auc  macro          0.611
#>  2 Fold02   pr_auc  macro          0.620
#>  3 Fold03   pr_auc  macro          0.689
#>  4 Fold04   pr_auc  macro          0.680
#>  5 Fold05   pr_auc  macro          0.620
#>  6 Fold06   pr_auc  macro          0.650
#>  7 Fold07   pr_auc  macro          0.607
#>  8 Fold08   pr_auc  macro          0.650
#>  9 Fold09   pr_auc  macro          0.628
#> 10 Fold10   pr_auc  macro          0.603

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  pr_auc(obs, VF:L, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   pr_auc  macro_weighted     0.746
#>  2 Fold02   pr_auc  macro_weighted     0.743
#>  3 Fold03   pr_auc  macro_weighted     0.789
#>  4 Fold04   pr_auc  macro_weighted     0.754
#>  5 Fold05   pr_auc  macro_weighted     0.737
#>  6 Fold06   pr_auc  macro_weighted     0.743
#>  7 Fold07   pr_auc  macro_weighted     0.748
#>  8 Fold08   pr_auc  macro_weighted     0.756
#>  9 Fold09   pr_auc  macro_weighted     0.711
#> 10 Fold10   pr_auc  macro_weighted     0.737

# Vector version
# Supply a matrix of class probabilities
fold1 <- hpc_cv |>
  filter(Resample == "Fold01")

pr_auc_vec(
   truth = fold1$obs,
   matrix(
     c(fold1$VF, fold1$F, fold1$M, fold1$L),
     ncol = 4
   )
)
#> [1] 0.6109931
```
