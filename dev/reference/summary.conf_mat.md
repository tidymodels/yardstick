# Summary Statistics for Confusion Matrices

Various statistical summaries of confusion matrices are produced and
returned in a tibble. These include those shown in the help pages for
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
[`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
and
[`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md),
among others.

## Usage

``` r
# S3 method for class 'conf_mat'
summary(
  object,
  prevalence = NULL,
  beta = 1,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
)
```

## Arguments

- object:

  An object of class
  [`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md).

- prevalence:

  A number in `(0, 1)` for the prevalence (i.e. prior) of the event. If
  left to the default, the data are used to derive this value.

- beta:

  A numeric value used to weight precision and recall for
  [`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md).

- estimator:

  One of: `"binary"`, `"macro"`, `"macro_weighted"`, or `"micro"` to
  specify the type of averaging to be done. `"binary"` is only relevant
  for the two class case. The other three are general methods for
  calculating multiclass metrics. The default will automatically choose
  `"binary"` or `"macro"` based on `estimate`.

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event". This argument is only
  applicable when `estimator = "binary"`. The default uses an internal
  helper that defaults to `"first"`.

- ...:

  Not currently used.

## Value

A tibble containing various classification metrics.

## Relevant Level

There is no common convention on which factor level should automatically
be considered the "event" or "positive" result when computing binary
classification metrics. In `yardstick`, the default is to use the
*first* level. To alter this, change the argument `event_level` to
`"second"` to consider the *last* level of the factor the level of
interest. For multiclass extensions involving one-vs-all comparisons
(such as macro averaging), this option is ignored and the "one" level is
always the relevant result.

## See also

[`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md)

## Examples

``` r
data("two_class_example")

cmat <- conf_mat(two_class_example, truth = "truth", estimate = "predicted")
summary(cmat)
#> # A tibble: 13 × 3
#>    .metric              .estimator .estimate
#>    <chr>                <chr>          <dbl>
#>  1 accuracy             binary         0.838
#>  2 kap                  binary         0.675
#>  3 sens                 binary         0.880
#>  4 spec                 binary         0.793
#>  5 ppv                  binary         0.819
#>  6 npv                  binary         0.861
#>  7 mcc                  binary         0.677
#>  8 j_index              binary         0.673
#>  9 bal_accuracy         binary         0.837
#> 10 detection_prevalence binary         0.554
#> 11 precision            binary         0.819
#> 12 recall               binary         0.880
#> 13 f_meas               binary         0.849
summary(cmat, prevalence = 0.70)
#> # A tibble: 13 × 3
#>    .metric              .estimator .estimate
#>    <chr>                <chr>          <dbl>
#>  1 accuracy             binary         0.838
#>  2 kap                  binary         0.675
#>  3 sens                 binary         0.880
#>  4 spec                 binary         0.793
#>  5 ppv                  binary         0.909
#>  6 npv                  binary         0.739
#>  7 mcc                  binary         0.677
#>  8 j_index              binary         0.673
#>  9 bal_accuracy         binary         0.837
#> 10 detection_prevalence binary         0.554
#> 11 precision            binary         0.819
#> 12 recall               binary         0.880
#> 13 f_meas               binary         0.849

library(dplyr)
library(tidyr)
data("hpc_cv")

# Compute statistics per resample then summarize
all_metrics <- hpc_cv |>
  group_by(Resample) |>
  conf_mat(obs, pred) |>
  mutate(summary_tbl = lapply(conf_mat, summary)) |>
  unnest(summary_tbl)

all_metrics |>
  group_by(.metric) |>
  summarise(
    mean = mean(.estimate, na.rm = TRUE),
    sd = sd(.estimate, na.rm = TRUE)
  )
#> # A tibble: 13 × 3
#>    .metric               mean       sd
#>    <chr>                <dbl>    <dbl>
#>  1 accuracy             0.709 2.47e- 2
#>  2 bal_accuracy         0.720 1.92e- 2
#>  3 detection_prevalence 0.25  9.25e-18
#>  4 f_meas               0.569 3.46e- 2
#>  5 j_index              0.439 3.85e- 2
#>  6 kap                  0.508 4.10e- 2
#>  7 mcc                  0.515 4.16e- 2
#>  8 npv                  0.896 1.11e- 2
#>  9 ppv                  0.633 3.87e- 2
#> 10 precision            0.633 3.87e- 2
#> 11 recall               0.560 3.09e- 2
#> 12 sens                 0.560 3.09e- 2
#> 13 spec                 0.879 9.67e- 3
```
