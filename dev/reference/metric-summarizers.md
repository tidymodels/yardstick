# Developer function for summarizing new metrics

These functions are useful alongside
[check_metric](https://yardstick.tidymodels.org/dev/reference/check_metric.md)
and
[yardstick_remove_missing](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
for implementing new custom metrics. These functions call the metric
function inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
or
[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
for `curve_metric_summarizer()`. See [Custom performance
metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
information.

## Usage

``` r
numeric_metric_summarizer(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

class_metric_summarizer(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

prob_metric_summarizer(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

ordered_prob_metric_summarizer(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

curve_metric_summarizer(
  name,
  fn,
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = NULL,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

dynamic_survival_metric_summarizer(
  name,
  fn,
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

static_survival_metric_summarizer(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

curve_survival_metric_summarizer(
  name,
  fn,
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

linear_pred_survival_metric_summarizer(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)

quantile_metric_summarizer(
  name,
  fn,
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  fn_options = list(),
  error_call = caller_env()
)
```

## Arguments

- name:

  A single character representing the name of the metric to use in the
  `tibble` output. This will be modified to include the type of
  averaging if appropriate.

- fn:

  The vector version of your custom metric function. It generally takes
  `truth`, `estimate`, `na_rm`, and any other extra arguments needed to
  calculate the metric.

- data:

  The data frame with `truth` and `estimate` columns passed in from the
  data frame version of your metric function that called the metric
  summarizer.

- truth:

  The unquoted column name corresponding to the `truth` column.

- estimate:

  Generally, the unquoted column name corresponding to the `estimate`
  column. For metrics that take multiple columns through `...` like
  class probability metrics, this is a result of
  [`dots_to_estimate()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md).

- ...:

  These dots are for future extensions and must be empty.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds. The removal is executed in
  [`yardstick_remove_missing()`](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md).

- case_weights:

  For metrics supporting case weights, an unquoted column name
  corresponding to case weights can be passed here. If not `NULL`, the
  case weights will be passed on to `fn` as the named argument
  `case_weights`.

- fn_options:

  A named list of metric specific options. These are spliced into the
  metric function call using `!!!` from `rlang`. The default results in
  nothing being spliced into the call.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- estimator:

  This can either be `NULL` for the default auto-selection of averaging
  (`"binary"` or `"macro"`), or a single character to pass along to the
  metric implementation describing the kind of averaging to use.

- event_level:

  This can either be `NULL` to use the default `event_level` value of
  the `fn` or a single string of either `"first"` or `"second"` to pass
  along describing which level should be considered the "event".

## Details

The following functions are generally called from the data frame version
of your metric function. They know how to call your metric over grouped
data frames and return a `tibble` consistent with other metrics.

- `numeric_metric_summarizer()`

- `class_metric_summarizer()`

- `prob_metric_summarizer()`

- `ordered_prob_metric_summarizer()`

- `curve_metric_summarizer()`

- `dynamic_survival_metric_summarizer()`

- `static_survival_metric_summarizer()`

- `curve_survival_metric_summarizer()`

- `linear_pred_survival_metric_summarizer()`

- `quantile_metric_summarizer()`

## See also

[check_metric](https://yardstick.tidymodels.org/dev/reference/check_metric.md)
[yardstick_remove_missing](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
[`finalize_estimator()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md)
[`dots_to_estimate()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md)
