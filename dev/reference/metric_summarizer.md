# Developer function for summarizing new metrics

**\[deprecated\]**

`metric_summarizer()` has been soft-deprecated as of yardstick 1.2.0.
Please switch to use
[`class_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
[`numeric_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
[`prob_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
or
[`curve_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md).

## Usage

``` r
metric_summarizer(
  metric_nm,
  metric_fn,
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  ...,
  metric_fn_options = list()
)
```

## Arguments

- metric_nm:

  A single character representing the name of the metric to use in the
  `tibble` output. This will be modified to include the type of
  averaging if appropriate.

- metric_fn:

  The vector version of your custom metric function. It generally takes
  `truth`, `estimate`, `na_rm`, and any other extra arguments needed to
  calculate the metric.

- data:

  The data frame with `truth` and `estimate` columns passed in from the
  data frame version of your metric function that called
  `metric_summarizer()`.

- truth:

  The unquoted column name corresponding to the `truth` column.

- estimate:

  Generally, the unquoted column name corresponding to the `estimate`
  column. For metrics that take multiple columns through `...` like
  class probability metrics, this is a result of
  [`dots_to_estimate()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md).

- estimator:

  For numeric metrics, this is left as `NULL` so averaging is not passed
  on to the metric function implementation. For classification metrics,
  this can either be `NULL` for the default auto-selection of averaging
  (`"binary"` or `"macro"`), or a single character to pass along to the
  metric implementation describing the kind of averaging to use.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds. The removal is executed in
  [`metric_vec_template()`](https://yardstick.tidymodels.org/dev/reference/metric_vec_template.md).

- event_level:

  For numeric metrics, this is left as `NULL` to prevent it from being
  passed on to the metric function implementation. For classification
  metrics, this can either be `NULL` to use the default `event_level`
  value of the `metric_fn` or a single string of either `"first"` or
  `"second"` to pass along describing which level should be considered
  the "event".

- case_weights:

  For metrics supporting case weights, an unquoted column name
  corresponding to case weights can be passed here. If not `NULL`, the
  case weights will be passed on to `metric_fn` as the named argument
  `case_weights`.

- ...:

  Currently not used. Metric specific options are passed in through
  `metric_fn_options`.

- metric_fn_options:

  A named list of metric specific options. These are spliced into the
  metric function call using `!!!` from `rlang`. The default results in
  nothing being spliced into the call.
