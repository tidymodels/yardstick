# Area under the receiver operator curve

`roc_auc()` is a metric that computes the area under the ROC curve. See
[`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md)
for the full curve.

## Usage

``` r
roc_auc(data, ...)

# S3 method for class 'data.frame'
roc_auc(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  options = list()
)

roc_auc_vec(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
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

  One of `"binary"`, `"hand_till"`, `"macro"`, or `"macro_weighted"` to
  specify the type of averaging to be done. `"binary"` is only relevant
  for the two class case. The others are general methods for calculating
  multiclass metrics. The default will automatically choose `"binary"`
  if `truth` is binary, `"hand_till"` if `truth` has \>2 levels and
  `case_weights` isn't specified, or `"macro"` if `truth` has \>2 levels
  and `case_weights` is specified (in which case `"hand_till"` isn't
  well-defined).

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

- options:

  `[deprecated]`

  No longer supported as of yardstick 1.0.0. If you pass something here
  it will be ignored with a warning.

  Previously, these were options passed on to `pROC::roc()`. If you need
  support for this, use the pROC package directly.

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

For `roc_auc_vec()`, a single `numeric` value (or `NA`).

## Details

Generally, an ROC AUC value is between `0.5` and `1`, with `1` being a
perfect prediction model. If your value is between `0` and `0.5`, then
this implies that you have meaningful information in your model, but it
is being applied incorrectly because doing the opposite of what the
model predicts would result in an AUC `>0.5`.

Note that you can't combine `estimator = "hand_till"` with
`case_weights`.

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

The default multiclass method for computing `roc_auc()` is to use the
method from Hand, Till, (2001). Unlike macro-averaging, this method is
insensitive to class distributions like the binary ROC AUC case.
Additionally, while other multiclass techniques will return `NA` if any
levels in `truth` occur zero times in the actual data, the Hand-Till
method will simply ignore those levels in the averaging calculation,
with a warning.

Macro and macro-weighted averaging are still provided, even though they
are not the default. In fact, macro-weighted averaging corresponds to
the same definition of multiclass AUC given by Provost and Domingos
(2001).

## References

Hand, Till (2001). "A Simple Generalisation of the Area Under the ROC
Curve for Multiple Class Classification Problems". *Machine Learning*.
Vol 45, Iss 2, pp 171-186.

Fawcett (2005). "An introduction to ROC analysis". *Pattern Recognition
Letters*. 27 (2006), pp 861-874.

Provost, F., Domingos, P., 2001. "Well-trained PETs: Improving
probability estimation trees", CeDER Working Paper \#IS-00-04, Stern
School of Business, New York University, NY, NY 10012.

## See also

[`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md)
for computing the full ROC curve.

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
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
roc_auc(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.939

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
  roc_auc(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc hand_till      0.813

# Change the first level of `obs` from `"VF"` to `"M"` to alter the
# event of interest. The class probability columns should be supplied
# in the same order as the levels.
hpc_cv |>
  filter(Resample == "Fold01") |>
  mutate(obs = relevel(obs, "M")) |>
  roc_auc(obs, M, VF:L)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc hand_till      0.813

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  roc_auc(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   roc_auc hand_till      0.813
#>  2 Fold02   roc_auc hand_till      0.817
#>  3 Fold03   roc_auc hand_till      0.869
#>  4 Fold04   roc_auc hand_till      0.849
#>  5 Fold05   roc_auc hand_till      0.811
#>  6 Fold06   roc_auc hand_till      0.836
#>  7 Fold07   roc_auc hand_till      0.825
#>  8 Fold08   roc_auc hand_till      0.846
#>  9 Fold09   roc_auc hand_till      0.828
#> 10 Fold10   roc_auc hand_till      0.812

# Weighted macro averaging
hpc_cv |>
  group_by(Resample) |>
  roc_auc(obs, VF:L, estimator = "macro_weighted")
#> # A tibble: 10 × 4
#>    Resample .metric .estimator     .estimate
#>    <chr>    <chr>   <chr>              <dbl>
#>  1 Fold01   roc_auc macro_weighted     0.880
#>  2 Fold02   roc_auc macro_weighted     0.873
#>  3 Fold03   roc_auc macro_weighted     0.906
#>  4 Fold04   roc_auc macro_weighted     0.867
#>  5 Fold05   roc_auc macro_weighted     0.866
#>  6 Fold06   roc_auc macro_weighted     0.865
#>  7 Fold07   roc_auc macro_weighted     0.868
#>  8 Fold08   roc_auc macro_weighted     0.865
#>  9 Fold09   roc_auc macro_weighted     0.841
#> 10 Fold10   roc_auc macro_weighted     0.869

# Vector version
# Supply a matrix of class probabilities
fold1 <- hpc_cv |>
  filter(Resample == "Fold01")

roc_auc_vec(
   truth = fold1$obs,
   matrix(
     c(fold1$VF, fold1$F, fold1$M, fold1$L),
     ncol = 4
   )
)
#> [1] 0.8131924
```
