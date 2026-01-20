# Concordance index for right-censored data

Compute the Concordance index for right-censored data

## Usage

``` r
concordance_survival(data, ...)

# S3 method for class 'data.frame'
concordance_survival(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)

concordance_survival_vec(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth` and `...`.

- ...:

  Not currently used.

- truth:

  The column identifier for the true survival result (that is created
  using
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, an
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object.

- estimate:

  The column identifier for the predicted time, this should be a numeric
  variables. This should be an unquoted column name although this
  argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a numeric
  vector.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in `data`. For
  `_vec()` functions, a numeric vector,
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
  or
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `concordance_survival_vec()`, a single `numeric` value (or `NA`).

## Details

Concordance is a metric that should be maximized. The output ranges from
0 to 1, with 1 indicating perfect concordance.

The concordance index is defined as the proportion of all comparable
pairs in which the predictions and outcomes are concordant.

Two observations are comparable if:

1.  both of the observations experienced an event (at different times),
    or

2.  the observation with the shorter observed survival time experienced
    an event, in which case the event-free subject “outlived” the other.

A pair is not comparable if they experienced events at the same time.

Concordance intuitively means that two samples were ordered correctly by
the model. More specifically, two samples are concordant, if the one
with a higher estimated risk score has a shorter actual survival time.

Larger values of the score are associated with better model performance.

## References

Harrell, F.E., Califf, R.M., Pryor, D.B., Lee, K.L., Rosati, R.A,
“Multivariable prognostic models: issues in developing models,
evaluating assumptions and adequacy, and measuring and reducing errors”,
Statistics in Medicine, 15(4), 361-87, 1996.

## Author

Emil Hvitfeldt

## Examples

``` r
concordance_survival(
  data = lung_surv,
  truth = surv_obj,
  estimate = .pred_time
)
#> # A tibble: 1 × 3
#>   .metric              .estimator .estimate
#>   <chr>                <chr>          <dbl>
#> 1 concordance_survival standard       0.637
```
