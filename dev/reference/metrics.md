# General Function to Estimate Performance

This function estimates one or more common performance estimates
depending on the class of `truth` (see **Value** below) and returns them
in a three column tibble. If you wish to modify the metrics used or how
they are used see
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md).

## Usage

``` r
metrics(data, ...)

# S3 method for class 'data.frame'
metrics(data, truth, estimate, ..., na_rm = TRUE, options = list())
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth`,
  `estimate`, and `...`.

- ...:

  A set of unquoted column names or one or more `dplyr` selector
  functions to choose which variables contain the class probabilities.
  If `truth` is binary, only 1 column should be selected, and it should
  correspond to the value of `event_level`. Otherwise, there should be
  as many columns as factor levels of `truth` and the ordering of the
  columns should be the same as the factor levels of `truth`.

- truth:

  The column identifier for the true results (that is `numeric` or
  `factor`). This should be an unquoted column name although this
  argument is passed by expression and support
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names).

- estimate:

  The column identifier for the predicted results (that is also
  `numeric` or `factor`). As with `truth` this can be specified
  different ways but the primary method is to use an unquoted variable
  name.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- options:

  `[deprecated]`

  No longer supported as of yardstick 1.0.0. If you pass something here
  it will be ignored with a warning.

  Previously, these were options passed on to `pROC::roc()`. If you need
  support for this, use the pROC package directly.

## Value

A three column tibble.

- When `truth` is a factor, there are rows for
  [`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md)
  and the Kappa statistic
  ([`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md)).

- When `truth` has two levels and 1 column of class probabilities is
  passed to `...`, there are rows for the two class versions of
  [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)
  and
  [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md).

- When `truth` has more than two levels and a full set of class
  probabilities are passed to `...`, there are rows for the multiclass
  version of
  [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)
  and the Hand Till generalization of
  [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md).

- When `truth` is numeric, there are rows for
  [`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
  [`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md), and
  [`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md).

## See also

[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md),
[`get_metrics()`](https://yardstick.tidymodels.org/dev/reference/get_metrics.md)

## Examples

``` r
# Accuracy and kappa
metrics(two_class_example, truth, predicted)
#> # A tibble: 2 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.838
#> 2 kap      binary         0.675

# Add on multinomal log loss and ROC AUC by specifying class prob columns
metrics(two_class_example, truth, predicted, Class1)
#> # A tibble: 4 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 accuracy    binary         0.838
#> 2 kap         binary         0.675
#> 3 mn_log_loss binary         0.328
#> 4 roc_auc     binary         0.939

# Regression metrics
metrics(solubility_test, truth = solubility, estimate = prediction)
#> # A tibble: 3 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard       0.722
#> 2 rsq     standard       0.879
#> 3 mae     standard       0.545

# Multiclass metrics work, but you cannot specify any averaging
# for roc_auc() besides the default, hand_till. Use the specific function
# if you need more customization
library(dplyr)

hpc_cv |>
  group_by(Resample) |>
  metrics(obs, pred, VF:L) |>
  print(n = 40)
#> # A tibble: 40 × 4
#>    Resample .metric     .estimator .estimate
#>    <chr>    <chr>       <chr>          <dbl>
#>  1 Fold01   accuracy    multiclass     0.726
#>  2 Fold02   accuracy    multiclass     0.712
#>  3 Fold03   accuracy    multiclass     0.758
#>  4 Fold04   accuracy    multiclass     0.712
#>  5 Fold05   accuracy    multiclass     0.712
#>  6 Fold06   accuracy    multiclass     0.697
#>  7 Fold07   accuracy    multiclass     0.675
#>  8 Fold08   accuracy    multiclass     0.721
#>  9 Fold09   accuracy    multiclass     0.673
#> 10 Fold10   accuracy    multiclass     0.699
#> 11 Fold01   kap         multiclass     0.533
#> 12 Fold02   kap         multiclass     0.512
#> 13 Fold03   kap         multiclass     0.594
#> 14 Fold04   kap         multiclass     0.511
#> 15 Fold05   kap         multiclass     0.514
#> 16 Fold06   kap         multiclass     0.486
#> 17 Fold07   kap         multiclass     0.454
#> 18 Fold08   kap         multiclass     0.531
#> 19 Fold09   kap         multiclass     0.454
#> 20 Fold10   kap         multiclass     0.492
#> 21 Fold01   mn_log_loss multiclass     0.734
#> 22 Fold02   mn_log_loss multiclass     0.808
#> 23 Fold03   mn_log_loss multiclass     0.705
#> 24 Fold04   mn_log_loss multiclass     0.747
#> 25 Fold05   mn_log_loss multiclass     0.799
#> 26 Fold06   mn_log_loss multiclass     0.766
#> 27 Fold07   mn_log_loss multiclass     0.927
#> 28 Fold08   mn_log_loss multiclass     0.855
#> 29 Fold09   mn_log_loss multiclass     0.861
#> 30 Fold10   mn_log_loss multiclass     0.821
#> 31 Fold01   roc_auc     hand_till      0.813
#> 32 Fold02   roc_auc     hand_till      0.817
#> 33 Fold03   roc_auc     hand_till      0.869
#> 34 Fold04   roc_auc     hand_till      0.849
#> 35 Fold05   roc_auc     hand_till      0.811
#> 36 Fold06   roc_auc     hand_till      0.836
#> 37 Fold07   roc_auc     hand_till      0.825
#> 38 Fold08   roc_auc     hand_till      0.846
#> 39 Fold09   roc_auc     hand_till      0.828
#> 40 Fold10   roc_auc     hand_till      0.812
```
