# Class metrics

Class metrics evaluate hard classification predictions where both
`truth` and `estimate` are factors. These metrics compare predicted
classes directly against the true classes.

## Input requirements

- `truth`: factor

- `estimate`: factor

## Available metrics

- [`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md):

  Direction: maximize. Range: \[0, 1\]

- [`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md):

  Direction: maximize. Range: \[0, 1\]

- [`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md):

  Direction: maximize. Range: \[0, 1\]

- [`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md):

  Direction: maximize. Range: \[0, 1\]

- [`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md):

  Direction: minimize. Range: \[0, 1\]

- [`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md):

  Direction: maximize. Range: \[-1, 1\]

- [`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md):

  Direction: maximize. Range: \[-1, 1\]

- [`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md):

  Direction: maximize. Range: \[-1, 1\]

- [`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md):

  Direction: minimize. Range: \[0, 1\]

- [`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md):

  Direction: maximize. Range: \[0, 1\]

- [`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md):

  Direction: maximize. Range: \[0, 1\]

- [`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md):

  Direction: maximize. Range: \[0, 1\]

- [`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md):

  Direction: maximize. Range: \[0, 1\]

- [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md):

  Direction: maximize. Range: \[0, 1\]

- [`sensitivity()`](https://yardstick.tidymodels.org/dev/reference/sens.md):

  Direction: maximize. Range: \[0, 1\]

- [`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md):

  Direction: maximize. Range: \[0, 1\]

- [`specificity()`](https://yardstick.tidymodels.org/dev/reference/spec.md):

  Direction: maximize. Range: \[0, 1\]

## See also

[prob-metrics](https://yardstick.tidymodels.org/dev/reference/prob-metrics.md)
for class probability metrics

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

accuracy(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.838
```
