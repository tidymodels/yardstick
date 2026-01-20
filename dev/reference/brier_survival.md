# Time-Dependent Brier score for right censored data

Compute the time-dependent Brier score for right censored data, which is
the mean squared error at time point `.eval_time`.

## Usage

``` r
brier_survival(data, ...)

# S3 method for class 'data.frame'
brier_survival(data, truth, ..., na_rm = TRUE, case_weights = NULL)

brier_survival_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth` and `...`.

- ...:

  The column identifier for the survival probabilities this should be a
  list column of data.frames corresponding to the output given when
  predicting with [censored](https://censored.tidymodels.org/) model.
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, the dots are
  not used.

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

- estimate:

  A list column of data.frames corresponding to the output given when
  predicting with [censored](https://censored.tidymodels.org/) model.
  See the details for more information regarding format.

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate`.

For an ungrouped data frame, the result has one row of values. For a
grouped data frame, the number of rows returned is the same as the
number of groups.

For `brier_survival_vec()`, a `numeric` vector same length as the input
argument `eval_time`. (or `NA`).

## Details

Brier survival score is a metric that should be minimized. The output
ranges from 0 to 1, with 0 indicating perfect predictions.

This formulation takes survival probability predictions at one or more
specific *evaluation times* and, for each time, computes the Brier
score. To account for censoring, inverse probability of censoring
weights (IPCW) are used in the calculations.

The column passed to `...` should be a list column with one element per
independent experiential unit (e.g. patient). The list column should
contain data frames with several columns:

- `.eval_time`: The time that the prediction is made.

- `.pred_survival`: The predicted probability of survival up to
  `.eval_time`

- `.weight_censored`: The case weight for the inverse probability of
  censoring.

The last column can be produced using
[`parsnip::.censoring_weights_graf()`](https://parsnip.tidymodels.org/reference/censoring_weights.html).
This corresponds to the weighting scheme of Graf *et al* (1999). The
internal data set `lung_surv` shows an example of the format.

This method automatically groups by the `.eval_time` argument.

Smaller values of the score are associated with better model
performance.

## References

E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment and
comparison of prognostic classification schemes for survival data,”
*Statistics in Medicine*, vol. 18, no. 17-18, pp. 2529–2545, 1999.

## See also

Other dynamic survival metrics:
[`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md),
[`roc_auc_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_auc_survival.md)

## Author

Emil Hvitfeldt

## Examples

``` r
# example code

library(dplyr)

lung_surv |>
  brier_survival(
    truth = surv_obj,
    .pred
  )
#> # A tibble: 5 × 4
#>   .metric        .estimator .eval_time .estimate
#>   <chr>          <chr>           <dbl>     <dbl>
#> 1 brier_survival standard          100     0.109
#> 2 brier_survival standard          200     0.194
#> 3 brier_survival standard          300     0.219
#> 4 brier_survival standard          400     0.222
#> 5 brier_survival standard          500     0.197
```
