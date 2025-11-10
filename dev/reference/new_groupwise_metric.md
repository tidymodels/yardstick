# Create groupwise metrics

Groupwise metrics quantify the disparity in value of a metric across a
number of groups. Groupwise metrics with a value of zero indicate that
the underlying metric is equal across groups. yardstick defines several
common fairness metrics using this function, such as
[`demographic_parity()`](https://yardstick.tidymodels.org/dev/reference/demographic_parity.md),
[`equal_opportunity()`](https://yardstick.tidymodels.org/dev/reference/equal_opportunity.md),
and
[`equalized_odds()`](https://yardstick.tidymodels.org/dev/reference/equalized_odds.md).

## Usage

``` r
new_groupwise_metric(fn, name, aggregate, direction = "minimize")
```

## Arguments

- fn:

  A yardstick metric function or metric set.

- name:

  The name of the metric to place in the `.metric` column of the output.

- aggregate:

  A function to summarize the generated metric set results. The function
  takes metric set results as the first argument and returns a single
  numeric giving the `.estimate` value as output. See the Value and
  Examples sections for example uses.

- direction:

  A string. One of:

  - `"maximize"`

  - `"minimize"`

  - `"zero"`

## Value

This function is a [function
factory](https://adv-r.hadley.nz/function-factories.html); its output is
itself a function. Further, the functions that this function outputs are
also function factories. More explicitly, this looks like:

    # a function with similar implementation to `demographic_parity()`:
    diff_range <- function(x) {diff(range(x$.estimate))}

    dem_parity <-
      new_groupwise_metric(
        fn = detection_prevalence,
        name = "dem_parity",
        aggregate = diff_range
      )

The outputted `dem_parity` is a function that takes one argument, `by`,
indicating the data-masked variable giving the sensitive feature.

When called with a `by` argument, `dem_parity` will return a yardstick
metric function like any other:

    dem_parity_by_gender <- dem_parity(gender)

Note that `dem_parity` doesn't take any arguments other than `by`, and
thus knows nothing about the data it will be applied to other than that
it ought to have a column with name `"gender"` in it.

The output `dem_parity_by_gender` is a metric function that takes the
same arguments as the function supplied as `fn`, in this case
`detection_prevalence`. It will thus interface like any other yardstick
function except that it will look for a `"gender"` column in the data
it's supplied.

In addition to the examples below, see the documentation on the return
value of fairness metrics like
[`demographic_parity()`](https://yardstick.tidymodels.org/dev/reference/demographic_parity.md),
[`equal_opportunity()`](https://yardstick.tidymodels.org/dev/reference/equal_opportunity.md),
or
[`equalized_odds()`](https://yardstick.tidymodels.org/dev/reference/equalized_odds.md)
to learn more about how the output of this function can be used.

## Details

Note that *all* yardstick metrics are group-aware in that, when passed
grouped data, they will return metric values calculated for each group.
When passed grouped data, groupwise metrics also return metric values
for each group, but those metric values are calculated by first
additionally grouping by the variable passed to `by` and then
summarizing the per-group metric estimates across groups using the
function passed as the `aggregate` argument. Learn more about grouping
behavior in yardstick using
[`vignette("grouping", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/grouping.md).

## Relevant Group Level

Additional arguments can be passed to the function outputted by the
function that this function outputs. That is:

    res_fairness <- new_groupwise_metric(...)
    res_by <- res_fairness(by)
    res_by(..., additional_arguments_to_aggregate = TRUE)

For finer control of how groups in `by` are treated, use the `aggregate`
argument.

## Examples

``` r
data(hpc_cv)

# `demographic_parity`, among other fairness metrics,
# is generated with `new_groupwise_metric()`:
diff_range <- function(x) {diff(range(x$.estimate))}
demographic_parity_ <-
  new_groupwise_metric(
    fn = detection_prevalence,
    name = "demographic_parity",
    aggregate = diff_range
  )

m_set <- metric_set(demographic_parity_(Resample))

m_set(hpc_cv, truth = obs, estimate = pred)
#> # A tibble: 1 × 4
#>   .metric            .by      .estimator .estimate
#>   <chr>              <chr>    <chr>          <dbl>
#> 1 demographic_parity Resample macro       2.78e-17

# the `post` argument can be used to accommodate a wide
# variety of parameterizations. to encode demographic
# parity as a ratio inside of a difference, for example:
ratio_range <- function(x, ...) {
  range <- range(x$.estimate)
  range[1] / range[2]
}

demographic_parity_ratio <-
  new_groupwise_metric(
    fn = detection_prevalence,
    name = "demographic_parity_ratio",
    aggregate = ratio_range
  )
```
