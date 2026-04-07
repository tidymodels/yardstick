# Multiclass Probability Predictions

Multiclass Probability Predictions

## Source

Kuhn, M., Johnson, K. (2013) *Applied Predictive Modeling*, Springer

## Value

- hpc_cv:

  a data frame

## Details

This data frame contains the predicted classes and class probabilities
for a linear discriminant analysis model fit to the HPC data set from
Kuhn and Johnson (2013). These data are the assessment sets from a
10-fold cross-validation scheme. The data column columns for the true
class (`obs`), the class prediction (`pred`) and columns for each class
probability (columns `VF`, `F`, `M`, and `L`). Additionally, a column
for the resample indicator is included.

## Examples

``` r
data(hpc_cv)
str(hpc_cv)
#> 'data.frame':    3467 obs. of  7 variables:
#>  $ obs     : Factor w/ 4 levels "VF","F","M","L": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ pred    : Factor w/ 4 levels "VF","F","M","L": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ VF      : num  0.914 0.938 0.947 0.929 0.942 ...
#>  $ F       : num  0.0779 0.0571 0.0495 0.0653 0.0543 ...
#>  $ M       : num  0.00848 0.00482 0.00316 0.00579 0.00381 ...
#>  $ L       : num  1.99e-05 1.01e-05 5.00e-06 1.56e-05 7.29e-06 ...
#>  $ Resample: chr  "Fold01" "Fold01" "Fold01" "Fold01" ...

# `obs` is a 4 level factor. The first level is `"VF"`, which is the
# "event of interest" by default in yardstick. See the Relevant Level
# section in any classification function (such as `?pr_auc`) to see how
# to change this.
levels(hpc_cv$obs)
#> [1] "VF" "F"  "M"  "L" 
```
