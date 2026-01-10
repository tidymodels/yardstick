# Compute weighted interval score

Weighted interval score (WIS), a well-known quantile-based approximation
of the commonly-used continuous ranked probability score (CRPS). WIS is
a proper score, and can be thought of as a distributional generalization
of absolute error. For example, see [Bracher et al.
(2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
of COVID-19 forecasting.

## Usage

``` r
weighted_interval_score(data, ...)

# S3 method for class 'data.frame'
weighted_interval_score(
  data,
  truth,
  estimate,
  quantile_levels = NULL,
  na_rm = TRUE,
  quantile_estimate_nas = c("impute", "drop", "propagate"),
  case_weights = NULL,
  ...
)

weighted_interval_score_vec(
  truth,
  estimate,
  quantile_levels = NULL,
  na_rm = FALSE,
  quantile_estimate_nas = c("impute", "drop", "propagate"),
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by the `truth` and
  `estimate` arguments.

- ...:

  Not Currently used.

- truth:

  The column identifier for the true class results (that is a
  `numeric`). This should be an unquoted column name although this
  argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `numeric`
  vector.

- estimate:

  The column identifier for the predicted class results (that is also
  `quantile_pred`). As with `truth` this can be specified different ways
  but the primary method is to use an unquoted variable name. For
  `_vec()` functions, a `quantile_pred` vector.

- quantile_levels:

  probabilities. If specified, the score will be computed at this set of
  levels. Otherwise, those present in `x` will be used. If
  `quantile_levels` do not exactly match those available in `x`, then
  some quantiles will have implicit missingness. Handling of these is
  determined by `quantile_estimate_nas`.

- na_rm:

  logical. If `TRUE`, missing values in `actual` or both implicit and
  explicit (values of `NA` present in `x`), will be ignored (dropped) in
  the calculation of the summary score. If `FALSE` (the default), any
  `NA`s will result in the summary being `NA`.

- quantile_estimate_nas:

  character. This argument applies only to `x`. It handles imputation of
  individual `quantile_levels` that are necessary to compute a score.
  Because each element of `x` is a
  [hardhat::quantile_pred](https://hardhat.tidymodels.org/reference/quantile_pred.html),
  it is possible for these to be missing for particular
  `quantile_levels`. There are a number of different possibilities for
  such missingness. The options are as follows:

  - For `"impute"`, both explicit and implicit missing values will be
    imputed using
    [`hardhat::impute_quantiles()`](https://hardhat.tidymodels.org/reference/impute_quantiles.html)
    prior to the calculation of the score. So the score will be `NA`
    only if imputation fails.

  - For `"drop"`, any explicit missing values will be removed before
    calculating the score for a particular prediction. This may be
    reasonable due to the weighting. For example, if the estimate has
    `quantile_levels = c(.25, .5, .75)` but the median is `NA` for a
    particular prediction, it may be reasonable to average the accuracy
    of `c(.25, .75)` for that prediction with others that don't have
    missingness. This option is only works if `quantile_levels = NULL`
    or is a subset of the `quantile_levels` in `x`.

  - For `"propagate"`, any missing value predictions will result in that
    element of `x` having a score of `NA`. If `na_rm = TRUE`, then these
    will be removed before averaging.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in `data`. For
  `_vec()` functions, a numeric vector,
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
  or
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

## Value

a vector of nonnegative scores.

## Examples

``` r
library(hardhat)

quantile_levels <- c(.2, .4, .6, .8)
pred1 <- 1:4
pred2 <- 8:11
preds <- quantile_pred(rbind(pred1, pred2), quantile_levels)
truth <- c(3.3, 7.1)
weighted_interval_score_vec(truth, preds)
#> [1] 1.275
weighted_interval_score_vec(truth, preds, quantile_levels = c(.25, .5, .75))
#> [1] 1.333333

# Missing value behaviours

preds_na <- quantile_pred(rbind(pred1, c(1, 2, NA, 4)), 1:4 / 5)
truth <- c(2.5, 2.5)
weighted_interval_score_vec(truth, preds_na)
#> [1] 0.5
weighted_interval_score_vec(truth, preds_na, quantile_levels = 1:9 / 10)
#> [1] 0.455656
try(weighted_interval_score_vec(
  truth,
  preds_na,
  quantile_levels = 1:9 / 10,
  quantile_estimate_nas = "drop"
))
#> Error in weighted_interval_score_vec(truth, preds_na, quantile_levels = 1:9/10,  : 
#>   When `quantile_levels` is not a subset of those available in
#> `estimate`, `quantile_estimate_nas` may not be `'drop'`.
weighted_interval_score_vec(
  truth,
  preds_na,
  quantile_levels = c(2, 3) / 5,
  quantile_estimate_nas = "drop"
)
#> [1] 0.4
weighted_interval_score_vec(
  truth, preds_na, na_rm = TRUE, quantile_estimate_nas = "propagate"
)
#> [1] 0.5
weighted_interval_score_vec(
  truth, preds_na, quantile_estimate_nas = "propagate"
)
#> [1] NA
```
