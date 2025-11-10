# Mean log loss for multinomial data

Compute the logarithmic loss of a classification model.

## Usage

``` r
mn_log_loss(data, ...)

# S3 method for class 'data.frame'
mn_log_loss(
  data,
  truth,
  ...,
  na_rm = TRUE,
  sum = FALSE,
  event_level = yardstick_event_level(),
  case_weights = NULL
)

mn_log_loss_vec(
  truth,
  estimate,
  na_rm = TRUE,
  sum = FALSE,
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

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- sum:

  A `logical`. Should the sum of the likelihood contributions be
  returned (instead of the mean value)?

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

For `mn_log_loss_vec()`, a single `numeric` value (or `NA`).

## Details

Log loss is a measure of the performance of a classification model. A
perfect model has a log loss of `0`.

Compared with
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
log loss takes into account the uncertainty in the prediction and gives
a more detailed view into the actual performance. For example, given two
input probabilities of `.6` and `.9` where both are classified as
predicting a positive value, say, `"Yes"`, the accuracy metric would
interpret them as having the same value. If the true output is `"Yes"`,
log loss penalizes `.6` because it is "less sure" of its result compared
to the probability of `.9`.

## Multiclass

Log loss has a known multiclass extension, and is simply the sum of the
log loss values for each class prediction. Because of this, no averaging
types are supported.

## See also

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Author

Max Kuhn

## Examples

``` r
# Two class
data("two_class_example")
mn_log_loss(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 mn_log_loss binary         0.328

# Multiclass
library(dplyr)
data(hpc_cv)

# You can use the col1:colN tidyselect syntax
hpc_cv |>
  filter(Resample == "Fold01") |>
  mn_log_loss(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 mn_log_loss multiclass     0.734

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  mn_log_loss(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric     .estimator .estimate
#>    <chr>    <chr>       <chr>          <dbl>
#>  1 Fold01   mn_log_loss multiclass     0.734
#>  2 Fold02   mn_log_loss multiclass     0.808
#>  3 Fold03   mn_log_loss multiclass     0.705
#>  4 Fold04   mn_log_loss multiclass     0.747
#>  5 Fold05   mn_log_loss multiclass     0.799
#>  6 Fold06   mn_log_loss multiclass     0.766
#>  7 Fold07   mn_log_loss multiclass     0.927
#>  8 Fold08   mn_log_loss multiclass     0.855
#>  9 Fold09   mn_log_loss multiclass     0.861
#> 10 Fold10   mn_log_loss multiclass     0.821


# Vector version
# Supply a matrix of class probabilities
fold1 <- hpc_cv |>
  filter(Resample == "Fold01")

mn_log_loss_vec(
  truth = fold1$obs,
  matrix(
    c(fold1$VF, fold1$F, fold1$M, fold1$L),
    ncol = 4
  )
)
#> [1] 0.7338423

# Supply `...` with quasiquotation
prob_cols <- levels(two_class_example$truth)
mn_log_loss(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 mn_log_loss binary         0.328
mn_log_loss(two_class_example, truth, !!prob_cols[1])
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 mn_log_loss binary         0.328
```
