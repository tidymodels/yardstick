# Costs function for poor classification

`classification_cost()` calculates the cost of a poor prediction based
on user-defined costs. The costs are multiplied by the estimated class
probabilities and the mean cost is returned.

## Usage

``` r
classification_cost(data, ...)

# S3 method for class 'data.frame'
classification_cost(
  data,
  truth,
  ...,
  costs = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
)

classification_cost_vec(
  truth,
  estimate,
  costs = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the columns specified by `truth` and `...`.

- ...:

  A set of unquoted column names or one or more `dplyr` selector
  functions to choose which variables contain the class probabilities.
  If `truth` is binary, only 1 column should be selected, and it should
  correspond to the value of `event_level`. Otherwise, there should be
  as many columns as factor levels of `truth` and the ordering of the
  columns should be the same as the factor levels of `truth`.

- truth:

  The column identifier for the true class results (that is a `factor`).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `factor`
  vector.

- costs:

  A data frame with columns `"truth"`, `"estimate"`, and `"cost"`.

  `"truth"` and `"estimate"` should be character columns containing
  unique combinations of the levels of the `truth` factor.

  `"costs"` should be a numeric column representing the cost that should
  be applied when the `"estimate"` is predicted, but the true result is
  `"truth"`.

  It is often the case that when `"truth" == "estimate"`, the cost is
  zero (no penalty for correct predictions).

  If any combinations of the levels of `truth` are missing, their costs
  are assumed to be zero.

  If `NULL`, equal costs are used, applying a cost of `0` to correct
  predictions, and a cost of `1` to incorrect predictions.

- na_rm:

  A `logical` value indicating whether `NA` values should be stripped
  before the computation proceeds.

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event". This argument is only
  applicable when `estimator = "binary"`. The default uses an internal
  helper that defaults to `"first"`.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in `data`. For
  `_vec()` functions, a numeric vector,
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
  or
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

- estimate:

  If `truth` is binary, a numeric vector of class probabilities
  corresponding to the "relevant" class. Otherwise, a matrix with as
  many columns as factor levels of `truth`. *It is assumed that these
  are in the same order as the levels of `truth`.*

## Value

A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1
row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For `class_cost_vec()`, a single `numeric` value (or `NA`).

## Details

As an example, suppose that there are three classes: `"A"`, `"B"`, and
`"C"`. Suppose there is a truly `"A"` observation with class
probabilities `A = 0.3 / B = 0.3 / C = 0.4`. Suppose that, when the true
result is class `"A"`, the costs for each class were
`A = 0 / B = 5 / C = 10`, penalizing the probability of incorrectly
predicting `"C"` more than predicting `"B"`. The cost for this
prediction would be `0.3 * 0 + 0.3 * 5 + 0.4 * 10`. This calculation is
done for each sample and the individual costs are averaged.

## See also

Other class probability metrics:
[`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md),
[`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md),
[`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md),
[`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md),
[`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md),
[`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md),
[`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
[`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
[`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)

## Author

Max Kuhn

## Examples

``` r
library(dplyr)

# ---------------------------------------------------------------------------
# Two class example
data(two_class_example)

# Assuming `Class1` is our "event", this penalizes false positives heavily
costs1 <- tribble(
  ~truth,   ~estimate, ~cost,
  "Class1", "Class2",  1,
  "Class2", "Class1",  2
)

# Assuming `Class1` is our "event", this penalizes false negatives heavily
costs2 <- tribble(
  ~truth,   ~estimate, ~cost,
  "Class1", "Class2",  2,
  "Class2", "Class1",  1
)

classification_cost(two_class_example, truth, Class1, costs = costs1)
#> # A tibble: 1 × 3
#>   .metric             .estimator .estimate
#>   <chr>               <chr>          <dbl>
#> 1 classification_cost binary         0.288

classification_cost(two_class_example, truth, Class1, costs = costs2)
#> # A tibble: 1 × 3
#>   .metric             .estimator .estimate
#>   <chr>               <chr>          <dbl>
#> 1 classification_cost binary         0.260

# ---------------------------------------------------------------------------
# Multiclass
data(hpc_cv)

# Define cost matrix from Kuhn and Johnson (2013)
hpc_costs <- tribble(
  ~estimate, ~truth, ~cost,
  "VF",      "VF",    0,
  "VF",      "F",     1,
  "VF",      "M",     5,
  "VF",      "L",    10,
  "F",       "VF",    1,
  "F",       "F",     0,
  "F",       "M",     5,
  "F",       "L",     5,
  "M",       "VF",    1,
  "M",       "F",     1,
  "M",       "M",     0,
  "M",       "L",     1,
  "L",       "VF",    1,
  "L",       "F",     1,
  "L",       "M",     1,
  "L",       "L",     0
)

# You can use the col1:colN tidyselect syntax
hpc_cv |>
  filter(Resample == "Fold01") |>
  classification_cost(obs, VF:L, costs = hpc_costs)
#> # A tibble: 1 × 3
#>   .metric             .estimator .estimate
#>   <chr>               <chr>          <dbl>
#> 1 classification_cost multiclass     0.779

# Groups are respected
hpc_cv |>
  group_by(Resample) |>
  classification_cost(obs, VF:L, costs = hpc_costs)
#> # A tibble: 10 × 4
#>    Resample .metric             .estimator .estimate
#>    <chr>    <chr>               <chr>          <dbl>
#>  1 Fold01   classification_cost multiclass     0.779
#>  2 Fold02   classification_cost multiclass     0.735
#>  3 Fold03   classification_cost multiclass     0.654
#>  4 Fold04   classification_cost multiclass     0.754
#>  5 Fold05   classification_cost multiclass     0.777
#>  6 Fold06   classification_cost multiclass     0.737
#>  7 Fold07   classification_cost multiclass     0.743
#>  8 Fold08   classification_cost multiclass     0.749
#>  9 Fold09   classification_cost multiclass     0.760
#> 10 Fold10   classification_cost multiclass     0.771
```
