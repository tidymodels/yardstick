# Time-Dependent ROC AUC for Censored Data

Compute the area under the ROC survival curve using predicted survival
probabilities that corresponds to different time points.

## Usage

``` r
roc_auc_survival(data, ...)

# S3 method for class 'data.frame'
roc_auc_survival(data, truth, ..., na_rm = TRUE, case_weights = NULL)

roc_auc_survival_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
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

For `roc_auc_survival_vec()`, a `numeric` vector same length as the
input argument `eval_time`. (or `NA`).

## Details

This formulation takes survival probability predictions at one or more
specific *evaluation times* and, for each time, computes the area under
the ROC curve. To account for censoring, inverse probability of
censoring weights (IPCW) are used in the calculations. See equation 7 of
section 4.3 in Blanche *at al* (2013) for the details.

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

Larger values of the score are associated with better model performance.

## References

Blanche, P., Dartigues, J.-F. and Jacqmin-Gadda, H. (2013), Review and
comparison of ROC curve estimators for a time-dependent outcome with
marker-dependent censoring. *Biom. J.*, 55: 687-704.

Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999),
Assessment and comparison of prognostic classification schemes for
survival data. *Statist. Med.*, 18: 2529-2545.

## See also

Compute the ROC survival curve with
[`roc_curve_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_curve_survival.md).

Other dynamic survival metrics:
[`brier_survival()`](https://yardstick.tidymodels.org/dev/reference/brier_survival.md),
[`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md)

## Author

Emil Hvitfeldt

## Examples

``` r
library(dplyr)

lung_surv |>
  roc_auc_survival(
    truth = surv_obj,
    .pred
  )
#> # A tibble: 5 × 4
#>   .metric          .estimator .eval_time .estimate
#>   <chr>            <chr>           <dbl>     <dbl>
#> 1 roc_auc_survival standard          100     0.659
#> 2 roc_auc_survival standard          200     0.678
#> 3 roc_auc_survival standard          300     0.684
#> 4 roc_auc_survival standard          400     0.630
#> 5 roc_auc_survival standard          500     0.643
```
