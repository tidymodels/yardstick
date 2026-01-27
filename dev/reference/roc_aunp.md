# Area under the ROC curve of each class against the rest, using the a priori class distribution

`roc_aunp()` is a multiclass metric that computes the area under the ROC
curve of each class against the rest, using the a priori class
distribution. This is equivalent to
`roc_auc(estimator = "macro_weighted")`.

## Usage

``` r
roc_aunp(data, ...)

# S3 method for class 'data.frame'
roc_aunp(data, truth, ..., na_rm = TRUE, case_weights = NULL, options = list())

roc_aunp_vec(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  options = list(),
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth` and `...`.

- ...:

  A set of unquoted column names or one or more `dplyr` selector
  functions to choose which variables contain the class probabilities.
  There should be as many columns as factor levels of `truth`.

- truth:

  The column identifier for the true class results (that is a `factor`).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `factor`
  vector.

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

- options:

  `[deprecated]`

  No longer supported as of yardstick 1.0.0. If you pass something here
  it will be ignored with a warning.

  Previously, these were options passed on to `pROC::roc()`. If you need
  support for this, use the pROC package directly.

- estimate:

  A matrix with as many columns as factor levels of `truth`. *It is
  assumed that these are in the same order as the levels of `truth`.*

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `roc_aunp_vec()`, a single `numeric` value (or `NA`).

## Details

ROC AUNP is a metric that should be maximized. The output ranges from 0
to 1, with 1 indicating perfect discrimination.

The formula used here is:

\$\$\text{ROC AUNP} = \sum\_{k=1}^{K} p_k \cdot \text{AUC}\_k\$\$

where \\p_k\\ is the proportion of observations in class \\k\\ and
\\\text{AUC}\_k\\ is the binary ROC AUC for class \\k\\ versus the rest.

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

This multiclass method for computing the area under the ROC curve uses
the a priori class distribution and is equivalent to
`roc_auc(estimator = "macro_weighted")`.

## References

Ferri, C., Hernández-Orallo, J., & Modroiu, R. (2009). "An experimental
comparison of performance measures for classification". *Pattern
Recognition Letters*. 30 (1), pp 27-38.

## See also

[All probability
metrics](https://yardstick.tidymodels.org/dev/reference/prob-metrics.md)

[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)
for computing the area under the ROC curve of each class against the
rest, using the uniform class distribution.

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Author

Julia Silge

## Examples

``` r
# Multiclass example

# `obs` is a 4 level factor. The first level is `"VF"`, which is the
# "event of interest" by default in yardstick. See the Relevant Level
# section above.
data(hpc_cv)

# You can use the col1:colN tidyselect syntax
library(dplyr)
hpc_cv |>
  filter(Resample == "Fold01") |>
  roc_aunp(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 roc_aunp macro          0.880

# Change the first level of `obs` from `"VF"` to `"M"` to alter the
# event of interest. The class probability columns should be supplied
# in the same order as the levels.
hpc_cv |>
  filter(Resample == "Fold01") |>
  mutate(obs = relevel(obs, "M")) |>
  roc_aunp(obs, M, VF:L)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 roc_aunp macro          0.880

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  roc_aunp(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   roc_aunp macro          0.880
#>  2 Fold02   roc_aunp macro          0.873
#>  3 Fold03   roc_aunp macro          0.906
#>  4 Fold04   roc_aunp macro          0.867
#>  5 Fold05   roc_aunp macro          0.866
#>  6 Fold06   roc_aunp macro          0.865
#>  7 Fold07   roc_aunp macro          0.868
#>  8 Fold08   roc_aunp macro          0.865
#>  9 Fold09   roc_aunp macro          0.841
#> 10 Fold10   roc_aunp macro          0.869

# Vector version
# Supply a matrix of class probabilities
fold1 <- hpc_cv |>
  filter(Resample == "Fold01")

roc_aunp_vec(
  truth = fold1$obs,
  matrix(
    c(fold1$VF, fold1$F, fold1$M, fold1$L),
    ncol = 4
  )
)
#> [1] 0.8795121
```
