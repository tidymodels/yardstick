# Class probability metrics

Class probability metrics evaluate soft classification predictions where
`truth` is a factor and `estimate` consists of class probability
columns. These metrics assess how well predicted probabilities match the
true class membership.

## Input requirements

- `truth`: factor

- `estimate` / `...`: numeric columns containing class probabilities

## Available metrics

- [`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md):

  Direction: maximize. Range: \[0, 1\]

- [`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md):

  Direction: minimize. Range: \[0, 1\]

- [`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md):

  Direction: minimize. Range: \[0, Inf\]

- [`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md):

  Direction: maximize. Range: \[0, 1\]

- [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md):

  Direction: minimize. Range: \[0, Inf\]

- [`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md):

  Direction: maximize. Range: \[0, 1\]

- [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md):

  Direction: maximize. Range: \[0, 1\]

- [`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md):

  Direction: maximize. Range: \[0, 1\]

- [`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md):

  Direction: maximize. Range: \[0, 1\]

## See also

[class-metrics](https://yardstick.tidymodels.org/dev/reference/class-metrics.md)
for hard classification metrics

[ordered-prob-metrics](https://yardstick.tidymodels.org/dev/reference/ordered-prob-metrics.md)
for ordered probability metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
data("two_class_example")

head(two_class_example)
#>    truth      Class1       Class2 predicted
#> 1 Class2 0.003589243 0.9964107574    Class2
#> 2 Class1 0.678621054 0.3213789460    Class1
#> 3 Class2 0.110893522 0.8891064779    Class2
#> 4 Class1 0.735161703 0.2648382969    Class1
#> 5 Class2 0.016239960 0.9837600397    Class2
#> 6 Class1 0.999275071 0.0007249286    Class1

roc_auc(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.939
```
