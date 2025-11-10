# Developer helpers

Helpers to be used alongside
[check_metric](https://yardstick.tidymodels.org/dev/reference/check_metric.md),
[yardstick_remove_missing](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
and [metric
summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
when creating new metrics. See [Custom performance
metrics](https://www.tidymodels.org/learn/develop/metrics/) for more
information.

## Usage

``` r
dots_to_estimate(data, ...)

get_weights(data, estimator)

finalize_estimator(
  x,
  estimator = NULL,
  metric_class = "default",
  call = caller_env()
)

finalize_estimator_internal(
  metric_dispatcher,
  x,
  estimator,
  call = caller_env()
)

validate_estimator(estimator, estimator_override = NULL, call = caller_env())
```

## Arguments

- data:

  A table with truth values as columns and predicted values as rows.

- ...:

  A set of unquoted column names or one or more `dplyr` selector
  functions to choose which variables contain the class probabilities.
  If `truth` is binary, only 1 column should be selected, and it should
  correspond to the value of `event_level`. Otherwise, there should be
  as many columns as factor levels of `truth` and the ordering of the
  columns should be the same as the factor levels of `truth`.

- estimator:

  Either `NULL` for auto-selection, or a single character for the type
  of estimator to use.

- x:

  The column used to autoselect the estimator. This is generally the
  `truth` column, but can also be a table if your metric has table
  methods.

- metric_class:

  A single character of the name of the metric to autoselect the
  estimator for. This should match the method name created for
  `finalize_estimator_internal()`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- metric_dispatcher:

  A simple dummy object with the class provided to `metric_class`. This
  is created and passed along for you.

- estimator_override:

  A character vector overriding the default allowed estimator list of
  `c("binary", "macro", "micro", "macro_weighted")`. Set this if your
  classification estimator does not support all of these methods.

## Dots -\> Estimate

**\[deprecated\]**

`dots_to_estimate()` is useful with class probability metrics that take
`...` rather than `estimate` as an argument. It constructs either a
single name if 1 input is provided to `...` or it constructs a quosure
where the expression constructs a matrix of as many columns as are
provided to `...`. These are eventually evaluated in the
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
call in
[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
and evaluate to either a vector or a matrix for further use in the
underlying vector functions.

## Weight Calculation

`get_weights()` accepts a confusion matrix and an `estimator` of type
`"macro"`, `"micro"`, or `"macro_weighted"` and returns the correct
weights. It is useful when creating multiclass metrics.

## Estimator Selection

`finalize_estimator()` is the engine for auto-selection of `estimator`
based on the type of `x`. Generally `x` is the `truth` column. This
function is called from the vector method of your metric.

`finalize_estimator_internal()` is an S3 generic that you should extend
for your metric if it does not implement *only* the following estimator
types: `"binary"`, `"macro"`, `"micro"`, and `"macro_weighted"`. If your
metric does support all of these, the default version of
`finalize_estimator_internal()` will autoselect `estimator`
appropriately. If you need to create a method, it should take the form:
`finalize_estimator_internal.metric_name`. Your method for
`finalize_estimator_internal()` should do two things:

1.  If `estimator` is `NULL`, autoselect the `estimator` based on the
    type of `x` and return a single character for the `estimator`.

2.  If `estimator` is not `NULL`, validate that it is an allowed
    `estimator` for your metric and return it.

If you are using the default for `finalize_estimator_internal()`, the
`estimator` is selected using the following heuristics:

1.  If `estimator` is not `NULL`, it is validated and returned
    immediately as no auto-selection is needed.

2.  If `x` is a:

    - `factor` - Then `"binary"` is returned if it has 2 levels,
      otherwise `"macro"` is returned.

    - `numeric` - Then `"binary"` is returned.

    - `table` - Then `"binary"` is returned if it has 2 columns,
      otherwise `"macro"` is returned. This is useful if you have
      `table` methods.

    - `matrix` - Then `"macro"` is returned.

## Estimator Validation

`validate_estimator()` is called from your metric specific method of
`finalize_estimator_internal()` and ensures that a user provided
estimator is of the right format and is one of the allowed values.

## See also

[metric-summarizers](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
[check_metric](https://yardstick.tidymodels.org/dev/reference/check_metric.md)
[yardstick_remove_missing](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
