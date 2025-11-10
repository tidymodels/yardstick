# Equal opportunity

Equal opportunity is satisfied when a model's predictions have the same
true positive and false negative rates across protected groups. A value
of 0 indicates parity across groups.

`equal_opportunity()` is calculated as the difference between the
largest and smallest value of
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md)
across groups.

Equal opportunity is sometimes referred to as equality of opportunity.

See the "Measuring Disparity" section for details on implementation.

## Usage

``` r
equal_opportunity(by)
```

## Arguments

- by:

  The column identifier for the sensitive feature. This should be an
  unquoted column name referring to a column in the un-preprocessed
  data.

## Value

This function outputs a yardstick *fairness metric* function. Given a
grouping variable `by`, `equal_opportunity()` will return a yardstick
metric function that is associated with the data-variable grouping `by`
and a post-processor. The outputted function will first generate a set
of sens metric values by group before summarizing across groups using
the post-processing function.

The outputted function only has a data frame method and is intended to
be used as part of a metric set.

## Measuring Disparity

By default, this function takes the difference in range of sens
`.estimate`s across groups. That is, the maximum pair-wise disparity
between groups is the return value of `equal_opportunity()`'s
`.estimate`.

For finer control of group treatment, construct a context-aware fairness
metric with the
[`new_groupwise_metric()`](https://yardstick.tidymodels.org/dev/reference/new_groupwise_metric.md)
function by passing a custom `aggregate` function:

    # the actual default `aggregate` is:
    diff_range <- function(x, ...) {diff(range(x$.estimate))}

    equal_opportunity_2 <-
      new_groupwise_metric(
        fn = sens,
        name = "equal_opportunity_2",
        aggregate = diff_range
      )

In [`aggregate()`](https://rdrr.io/r/stats/aggregate.html), `x` is the
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
output with sens values for each group, and `...` gives additional
arguments (such as a grouping level to refer to as the "baseline") to
pass to the function outputted by `equal_opportunity_2()` for context.

## References

Hardt, M., Price, E., & Srebro, N. (2016). "Equality of opportunity in
supervised learning". Advances in neural information processing systems,
29.

Verma, S., & Rubin, J. (2018). "Fairness definitions explained". In
Proceedings of the international workshop on software fairness (pp.
1-7).

Bird, S., Dudík, M., Edgar, R., Horn, B., Lutz, R., Milan, V., ... &
Walker, K. (2020). "Fairlearn: A toolkit for assessing and improving
fairness in AI". Microsoft, Tech. Rep. MSR-TR-2020-32.

## See also

Other fairness metrics:
[`demographic_parity()`](https://yardstick.tidymodels.org/dev/reference/demographic_parity.md),
[`equalized_odds()`](https://yardstick.tidymodels.org/dev/reference/equalized_odds.md)

## Examples

``` r
library(dplyr)

data(hpc_cv)

head(hpc_cv)
#>   obs pred        VF          F           M            L Resample
#> 1  VF   VF 0.9136340 0.07786694 0.008479147 1.991225e-05   Fold01
#> 2  VF   VF 0.9380672 0.05710623 0.004816447 1.011557e-05   Fold01
#> 3  VF   VF 0.9473710 0.04946767 0.003156287 4.999849e-06   Fold01
#> 4  VF   VF 0.9289077 0.06528949 0.005787179 1.564496e-05   Fold01
#> 5  VF   VF 0.9418764 0.05430830 0.003808013 7.294581e-06   Fold01
#> 6  VF   VF 0.9510978 0.04618223 0.002716177 3.841455e-06   Fold01

# evaluate `equal_opportunity()` by Resample
m_set <- metric_set(equal_opportunity(Resample))

# use output like any other metric set
hpc_cv |>
  m_set(truth = obs, estimate = pred)
#> # A tibble: 1 × 4
#>   .metric           .by      .estimator .estimate
#>   <chr>             <chr>    <chr>          <dbl>
#> 1 equal_opportunity Resample macro          0.103

# can mix fairness metrics and regular metrics
m_set_2 <- metric_set(sens, equal_opportunity(Resample))

hpc_cv |>
  m_set_2(truth = obs, estimate = pred)
#> # A tibble: 2 × 4
#>   .metric           .estimator .estimate .by     
#>   <chr>             <chr>          <dbl> <chr>   
#> 1 sens              macro          0.560 NA      
#> 2 equal_opportunity macro          0.103 Resample
```
