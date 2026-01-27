# Ordered probability metrics

Ordered probability metrics evaluate predictions for ordered factor
outcomes where the class probabilities should respect the natural
ordering of the levels.

## Input requirements

- `truth`: ordered factor

- `estimate` / `...`: numeric columns containing class probabilities

## Available metrics

- [`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md):

  Direction: minimize. Range: \[0, 1\]

## See also

[class-metrics](https://yardstick.tidymodels.org/dev/reference/class-metrics.md)
for hard classification metrics

[prob-metrics](https://yardstick.tidymodels.org/dev/reference/prob-metrics.md)
for class probability metrics

[`vignette("metric-types")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md)
for an overview of all metric types

## Examples

``` r
# Example with an ordered factor
set.seed(1)
df <- data.frame(
  truth = ordered(sample(1:3, 20, replace = TRUE)),
  prob_1 = runif(20),
  prob_2 = runif(20),
  prob_3 = runif(20)
)
# Normalize probabilities
df[2:4] <- df[2:4] / rowSums(df[2:4])

ranked_prob_score(df, truth, prob_1:prob_3)
#> # A tibble: 1 × 3
#>   .metric           .estimator .estimate
#>   <chr>             <chr>          <dbl>
#> 1 ranked_prob_score multiclass     0.212
```
