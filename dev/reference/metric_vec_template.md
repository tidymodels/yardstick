# Developer function for calling new metrics

**\[deprecated\]**

`metric_vec_template()` has been soft-deprecated as of yardstick 1.2.0.
Please switch to use
[check_metric](https://yardstick.tidymodels.org/dev/reference/check_metric.md)
and
[yardstick_remove_missing](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
functions.

## Usage

``` r
metric_vec_template(
  metric_impl,
  truth,
  estimate,
  na_rm = TRUE,
  cls = "numeric",
  estimator = NULL,
  case_weights = NULL,
  ...
)
```

## Arguments

- metric_impl:

  The core implementation function of your custom metric. This core
  implementation function is generally defined inside the vector method
  of your metric function.

- truth:

  The realized vector of `truth`. This is either a factor or a numeric.

- estimate:

  The realized `estimate` result. This is either a numeric vector, a
  factor vector, or a numeric matrix (in the case of multiple class
  probability columns) depending on your metric function.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds. `NA` values are removed before
  getting to your core implementation function so you do not have to
  worry about handling them yourself. If `na_rm=FALSE` and any `NA`
  values exist, then `NA` is automatically returned.

- cls:

  A character vector of length 1 or 2 corresponding to the class that
  `truth` and `estimate` should be, respectively. If `truth` and
  `estimate` are of the same class, just supply a vector of length 1. If
  they are different, supply a vector of length 2. For matrices, it is
  best to supply `"numeric"` as the class to check here.

- estimator:

  The type of averaging to use. By this point, the averaging type should
  be finalized, so this should be a character vector of length 1\\ By
  default, this character value is required to be one of: `"binary"`,
  `"macro"`, `"micro"`, or `"macro_weighted"`. If your metric allows
  more or less averaging methods, override this with
  `averaging_override`.

- case_weights:

  Optionally, the realized case weights, as a numeric vector. This must
  be the same length as `truth`, and will be considered in the `na_rm`
  checks. If supplied, this will be passed on to `metric_impl` as the
  named argument `case_weights`.

- ...:

  Extra arguments to your core metric function, `metric_impl`, can
  technically be passed here, but generally the extra args are added
  through R's scoping rules because the core metric function is created
  on the fly when the vector method is called.
