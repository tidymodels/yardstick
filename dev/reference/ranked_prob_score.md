# Ranked probability scores for ordinal classification models

Compute the ranked probability score (RPS) for a classification model
using ordered classes.

## Usage

``` r
ranked_prob_score(data, ...)

# S3 method for class 'data.frame'
ranked_prob_score(data, truth, ..., na_rm = TRUE, case_weights = NULL)

ranked_prob_score_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

  The column identifier for the true class results (that is an *ordered*
  `factor`). This should be an unquoted column name although this
  argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a factor
  vector with class `ordered`.

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

- estimate:

  A matrix with as many columns as factor levels of `truth`. *It is
  assumed that these are in the same order as the levels of `truth`.*

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `ranked_prob_score_vec()`, a single `numeric` value (or `NA`).

## Details

Ranked probability score is a metric that should be minimized. The
output ranges from 0 to 1, with 0 indicating perfect predictions.

The ranked probability score is a Brier score for ordinal data that uses
the *cumulative* probability of an event (i.e. `Pr[class <= i]` for `i`
= 1, 2, ..., `C` classes). These probabilities are compared to
indicators for the truth being less than or equal to class `i`.

Since the cumulative sum of a vector of probability predictions add up
to one, there is an embedded redundancy in the data. For this reason,
the raw mean is divided by the number of classes minus one.

Smaller values of the score are associated with better model
performance.

## Multiclass

Ranked probability scores can be computed in the same way for any number
of classes. Because of this, no averaging types are supported.

## References

Wilks, D. S. (2011). *Statistical Methods in the Atmospheric Sciences*.
Academic press. (see Chapter 7)

Janitza, S., Tutz, G., & Boulesteix, A. L. (2016). Random forest for
ordinal responses: prediction and variable selection. Computational
Statistics and Data Analysis, 96, 57-73. (see Section 2)

Lechner, M., & Okasa, G. (2019). Random forest estimation of the ordered
choice model. arXiv preprint arXiv:1907.02436. (see Section 5)

## See also

[All ordered probability
metrics](https://yardstick.tidymodels.org/dev/reference/ordered-prob-metrics.md)

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Author

Max Kuhn

## Examples

``` r
library(dplyr)
data(hpc_cv)

hpc_cv$obs <- as.ordered(hpc_cv$obs)

# You can use the col1:colN tidyselect syntax
hpc_cv |>
  filter(Resample == "Fold01") |>
  ranked_prob_score(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric           .estimator .estimate
#>   <chr>             <chr>          <dbl>
#> 1 ranked_prob_score multiclass    0.0810

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  ranked_prob_score(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric           .estimator .estimate
#>    <chr>    <chr>             <chr>          <dbl>
#>  1 Fold01   ranked_prob_score multiclass    0.0810
#>  2 Fold02   ranked_prob_score multiclass    0.0870
#>  3 Fold03   ranked_prob_score multiclass    0.0713
#>  4 Fold04   ranked_prob_score multiclass    0.0825
#>  5 Fold05   ranked_prob_score multiclass    0.0876
#>  6 Fold06   ranked_prob_score multiclass    0.0833
#>  7 Fold07   ranked_prob_score multiclass    0.0926
#>  8 Fold08   ranked_prob_score multiclass    0.0862
#>  9 Fold09   ranked_prob_score multiclass    0.0955
#> 10 Fold10   ranked_prob_score multiclass    0.0897
```
