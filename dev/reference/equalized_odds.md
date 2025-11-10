# Equalized odds

Equalized odds is satisfied when a model's predictions have the same
false positive, true positive, false negative, and true negative rates
across protected groups. A value of 0 indicates parity across groups.

By default, this function takes the maximum difference in range of
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) and
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
`.estimate`s across groups. That is, the maximum pair-wise disparity in
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) or
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
between groups is the return value of `equalized_odds()`'s `.estimate`.

Equalized odds is sometimes referred to as conditional procedure
accuracy equality or disparate mistreatment.

See the "Measuring disparity" section for details on implementation.

## Usage

``` r
equalized_odds(by)
```

## Arguments

- by:

  The column identifier for the sensitive feature. This should be an
  unquoted column name referring to a column in the un-preprocessed
  data.

## Value

This function outputs a yardstick *fairness metric* function. Given a
grouping variable `by`, `equalized_odds()` will return a yardstick
metric function that is associated with the data-variable grouping `by`
and a post-processor. The outputted function will first generate a set
of [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md)
and [`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
metric values by group before summarizing across groups using the
post-processing function.

The outputted function only has a data frame method and is intended to
be used as part of a metric set.

## Measuring Disparity

For finer control of group treatment, construct a context-aware fairness
metric with the
[`new_groupwise_metric()`](https://yardstick.tidymodels.org/dev/reference/new_groupwise_metric.md)
function by passing a custom `aggregate` function:

    # see yardstick:::max_positive_rate_diff for the actual `aggregate()`
    diff_range <- function(x, ...) {diff(range(x$.estimate))}

    equalized_odds_2 <-
      new_groupwise_metric(
        fn = metric_set(sens, spec),
        name = "equalized_odds_2",
        aggregate = diff_range
      )

In [`aggregate()`](https://rdrr.io/r/stats/aggregate.html), `x` is the
[`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
output with
[`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) and
[`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
values for each group, and `...` gives additional arguments (such as a
grouping level to refer to as the "baseline") to pass to the function
outputted by `equalized_odds_2()` for context.

## References

Agarwal, A., Beygelzimer, A., Dudik, M., Langford, J., & Wallach, H.
(2018). "A Reductions Approach to Fair Classification." Proceedings of
the 35th International Conference on Machine Learning, in Proceedings of
Machine Learning Research. 80:60-69.

Verma, S., & Rubin, J. (2018). "Fairness definitions explained". In
Proceedings of the international workshop on software fairness (pp.
1-7).

Bird, S., Dudík, M., Edgar, R., Horn, B., Lutz, R., Milan, V., ... &
Walker, K. (2020). "Fairlearn: A toolkit for assessing and improving
fairness in AI". Microsoft, Tech. Rep. MSR-TR-2020-32.

## See also

Other fairness metrics:
[`demographic_parity()`](https://yardstick.tidymodels.org/dev/reference/demographic_parity.md),
[`equal_opportunity()`](https://yardstick.tidymodels.org/dev/reference/equal_opportunity.md)

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

# evaluate `equalized_odds()` by Resample
m_set <- metric_set(equalized_odds(Resample))

# use output like any other metric set
hpc_cv |>
  m_set(truth = obs, estimate = pred)
#> # A tibble: 1 × 4
#>   .metric        .by      .estimator .estimate
#>   <chr>          <chr>    <chr>          <dbl>
#> 1 equalized_odds Resample macro          0.103

# can mix fairness metrics and regular metrics
m_set_2 <- metric_set(sens, equalized_odds(Resample))

hpc_cv |>
  m_set_2(truth = obs, estimate = pred)
#> # A tibble: 2 × 4
#>   .metric        .estimator .estimate .by     
#>   <chr>          <chr>          <dbl> <chr>   
#> 1 sens           macro          0.560 NA      
#> 2 equalized_odds macro          0.103 Resample
```
