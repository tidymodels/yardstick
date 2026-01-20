# Royston-Sauerbei D statistic

Compute the Royston-Sauerbei D statistic

## Usage

``` r
royston_survival(data, ...)

# S3 method for class 'data.frame'
royston_survival(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

royston_survival_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

  The column identifier for the predicted linear predictor, this should
  be a numeric variable. This should be an unquoted column name although
  this argument is passed by expression and supports
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

For `royston_survival_vec()`, a single `numeric` value (or `NA`).

## Details

Royston D statistic is a metric that should be maximized. The output
ranges from 0 to 1, with 1 indicating perfect prognostic separation.

Royston and Sauerbrei proposed \$R^2_D\$ as a measure of explained
variation on the log relative hazard scale based on the authors’ D
statistic. D measures prognostic separation of survival curves, and is
closely related to the standard deviation of the prognostic index.

Larger values of the score are associated with better model performance.

## References

Royston, P., Sauerbrei, W., "A new measure of prognostic separation in
survival data", Statistics in Medicine, 23, 723-748, 2004.

## Author

Hannah Frick

## Examples

``` r
royston_survival(
  data = lung_surv,
  truth = surv_obj,
  estimate = .pred_linear_pred
)
#> # A tibble: 1 × 3
#>   .metric          .estimator .estimate
#>   <chr>            <chr>          <dbl>
#> 1 royston_survival standard       0.116
```
