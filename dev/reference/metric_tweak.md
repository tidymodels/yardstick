# Tweak a metric function

`metric_tweak()` allows you to tweak an existing metric `.fn`, giving it
a new `.name` and setting new optional argument defaults through `...`.
It is similar to
[`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html),
but is designed specifically for yardstick metrics.

`metric_tweak()` is especially useful when constructing a
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
for tuning with the tune package. After the metric set has been
constructed, there is no way to adjust the value of any optional
arguments (such as `beta` in
[`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md)).
Using `metric_tweak()`, you can set optional arguments to custom values
ahead of time, before they go into the metric set.

## Usage

``` r
metric_tweak(.name, .fn, ...)
```

## Arguments

- .name:

  A single string giving the name of the new metric. This will be used
  in the `".metric"` column of the output.

- .fn:

  An existing yardstick metric function to tweak.

- ...:

  Name-value pairs specifying which optional arguments to override and
  the values to replace them with.

  Arguments `data`, `truth`, and `estimate` are considered *protected*,
  and cannot be overridden, but all other optional arguments can be
  altered.

## Value

A tweaked version of `.fn`, updated to use new defaults supplied in
`...`.

## Details

The function returned from `metric_tweak()` only takes `...` as
arguments, which are passed through to the original `.fn`. Passing
`data`, `truth`, and `estimate` through by position should generally be
safe, but it is recommended to pass any other optional arguments through
by name to ensure that they are evaluated correctly.

## Examples

``` r
mase12 <- metric_tweak("mase12", mase, m = 12)

# Defaults to `m = 1`
mase(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mase    standard        3.56

# Updated to use `m = 12`. `mase12()` has this set already.
mase(solubility_test, solubility, prediction, m = 12)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mase    standard       0.582
mase12(solubility_test, solubility, prediction)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mase12  standard       0.582

# This is most useful to set optional argument values ahead of time when
# using a metric set
mase10 <- metric_tweak("mase10", mase, m = 10)
metrics <- metric_set(mase, mase10, mase12)
metrics(solubility_test, solubility, prediction)
#> # A tibble: 3 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mase    standard       3.56 
#> 2 mase10  standard       0.664
#> 3 mase12  standard       0.582
```
