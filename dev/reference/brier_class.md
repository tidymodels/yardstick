# Brier score for classification models

Compute the Brier score for a classification model.

## Usage

``` r
brier_class(data, ...)

# S3 method for class 'data.frame'
brier_class(data, truth, ..., na_rm = TRUE, case_weights = NULL)

brier_class_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `brier_class_vec()`, a single `numeric` value (or `NA`).

## Details

Brier score is a metric that should be minimized. The output ranges from
0 to 1, with 0 indicating perfect predictions.

The Brier score is analogous to the mean squared error in regression
models. The difference between a binary indicator for a class and its
corresponding class probability are squared and averaged.

This function uses the convention in Kruppa *et al* (2014) and divides
the result by two.

Smaller values of the score are associated with better model
performance.

## Multiclass

Brier scores can be computed in the same way for any number of classes.
Because of this, no averaging types are supported.

## References

Kruppa, J., Liu, Y., Diener, H.-C., Holste, T., Weimar, C., Koonig, I.
R., and Ziegler, A. (2014) Probability estimation with machine learning
methods for dichotomous and multicategory outcome: Applications.
Biometrical Journal, 56 (4): 564-583.

## See also

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
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
brier_class(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 brier_class binary         0.106

# Multiclass
library(dplyr)
data(hpc_cv)

# You can use the col1:colN tidyselect syntax
hpc_cv |>
  filter(Resample == "Fold01") |>
  brier_class(obs, VF:L)
#> # A tibble: 1 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 brier_class multiclass     0.202

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  brier_class(obs, VF:L)
#> # A tibble: 10 × 4
#>    Resample .metric     .estimator .estimate
#>    <chr>    <chr>       <chr>          <dbl>
#>  1 Fold01   brier_class multiclass     0.202
#>  2 Fold02   brier_class multiclass     0.215
#>  3 Fold03   brier_class multiclass     0.177
#>  4 Fold04   brier_class multiclass     0.204
#>  5 Fold05   brier_class multiclass     0.213
#>  6 Fold06   brier_class multiclass     0.214
#>  7 Fold07   brier_class multiclass     0.221
#>  8 Fold08   brier_class multiclass     0.209
#>  9 Fold09   brier_class multiclass     0.235
#> 10 Fold10   brier_class multiclass     0.218
```
