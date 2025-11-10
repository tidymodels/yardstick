# Combine metric functions

`metric_set()` allows you to combine multiple metric functions together
into a new function that calculates all of them at once.

## Usage

``` r
metric_set(...)
```

## Arguments

- ...:

  The bare names of the functions to be included in the metric set.

## Details

All functions must be either:

- Only numeric metrics

- A mix of class metrics or class prob metrics

- A mix of dynamic, integrated, and static survival metrics

For instance,
[`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md) can
be used with
[`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md) because
they are numeric metrics, but not with
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md)
because it is a classification metric. But
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md)
can be used with
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md).

The returned metric function will have a different argument list
depending on whether numeric metrics or a mix of class/prob metrics were
passed in.

    # Numeric metric set signature:
    fn(
      data,
      truth,
      estimate,
      na_rm = TRUE,
      case_weights = NULL,
      ...
    )

    # Class / prob metric set signature:
    fn(
      data,
      truth,
      ...,
      estimate,
      estimator = NULL,
      na_rm = TRUE,
      event_level = yardstick_event_level(),
      case_weights = NULL
    )

    # Dynamic / integrated / static survival metric set signature:
    fn(
      data,
      truth,
      ...,
      estimate,
      na_rm = TRUE,
      case_weights = NULL
    )

When mixing class and class prob metrics, pass in the hard predictions
(the factor column) as the named argument `estimate`, and the soft
predictions (the class probability columns) as bare column names or
`tidyselect` selectors to `...`.

When mixing dynamic, integrated, and static survival metrics, pass in
the time predictions as the named argument `estimate`, and the survival
predictions as bare column names or `tidyselect` selectors to `...`.

If
[`metric_tweak()`](https://yardstick.tidymodels.org/dev/reference/metric_tweak.md)
has been used to "tweak" one of these arguments, like `estimator` or
`event_level`, then the tweaked version wins. This allows you to set the
estimator on a metric by metric basis and still use it in a
`metric_set()`.

## See also

[`metrics()`](https://yardstick.tidymodels.org/dev/reference/metrics.md)

## Examples

``` r
library(dplyr)

# Multiple regression metrics
multi_metric <- metric_set(rmse, rsq, ccc)

# The returned function has arguments:
# fn(data, truth, estimate, na_rm = TRUE, ...)
multi_metric(solubility_test, truth = solubility, estimate = prediction)
#> # A tibble: 3 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard       0.722
#> 2 rsq     standard       0.879
#> 3 ccc     standard       0.937

# Groups are respected on the new metric function
class_metrics <- metric_set(accuracy, kap)

hpc_cv |>
  group_by(Resample) |>
  class_metrics(obs, estimate = pred)
#> # A tibble: 20 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
#> 11 Fold01   kap      multiclass     0.533
#> 12 Fold02   kap      multiclass     0.512
#> 13 Fold03   kap      multiclass     0.594
#> 14 Fold04   kap      multiclass     0.511
#> 15 Fold05   kap      multiclass     0.514
#> 16 Fold06   kap      multiclass     0.486
#> 17 Fold07   kap      multiclass     0.454
#> 18 Fold08   kap      multiclass     0.531
#> 19 Fold09   kap      multiclass     0.454
#> 20 Fold10   kap      multiclass     0.492

# ---------------------------------------------------------------------------

# If you need to set options for certain metrics,
# do so by wrapping the metric and setting the options inside the wrapper,
# passing along truth and estimate as quoted arguments.
# Then add on the function class of the underlying wrapped function,
# and the direction of optimization.
ccc_with_bias <- function(data, truth, estimate, na_rm = TRUE, ...) {
  ccc(
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    # set bias = TRUE
    bias = TRUE,
    na_rm = na_rm,
    ...
  )
}

# Use `new_numeric_metric()` to formalize this new metric function
ccc_with_bias <- new_numeric_metric(ccc_with_bias, "maximize")

multi_metric2 <- metric_set(rmse, rsq, ccc_with_bias)

multi_metric2(solubility_test, truth = solubility, estimate = prediction)
#> # A tibble: 3 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard       0.722
#> 2 rsq     standard       0.879
#> 3 ccc     standard       0.937

# ---------------------------------------------------------------------------
# A class probability example:

# Note that, when given class or class prob functions,
# metric_set() returns a function with signature:
# fn(data, truth, ..., estimate)
# to be able to mix class and class prob metrics.

# You must provide the `estimate` column by explicitly naming
# the argument

class_and_probs_metrics <- metric_set(roc_auc, pr_auc, accuracy)

hpc_cv |>
  group_by(Resample) |>
  class_and_probs_metrics(obs, VF:L, estimate = pred)
#> # A tibble: 30 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
#> # ℹ 20 more rows
```
