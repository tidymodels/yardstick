# Developer function for handling missing values in new metrics

`yardstick_remove_missing()`, and `yardstick_any_missing()` are useful
alongside the
[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
functions for implementing new custom metrics.
`yardstick_remove_missing()` removes any observations that contains
missing values across, `truth`, `estimate` and `case_weights`.
`yardstick_any_missing()` returns `FALSE` if there is any missing values
in the inputs.

## Usage

``` r
yardstick_remove_missing(truth, estimate, case_weights)

yardstick_any_missing(truth, estimate, case_weights)
```

## Arguments

- truth, estimate:

  Vectors of the same length.

- case_weights:

  A vector of the same length as `truth` and `estimate`, or `NULL` if
  case weights are not being used.

## See also

[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
