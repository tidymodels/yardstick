# Solubility Predictions from MARS Model

Solubility Predictions from MARS Model

## Source

Kuhn, M., Johnson, K. (2013) *Applied Predictive Modeling*, Springer

## Value

- solubility_test:

  a data frame

## Details

For the solubility data in Kuhn and Johnson (2013), these data are the
test set results for the MARS model. The observed solubility (in column
`solubility`) and the model results (`prediction`) are contained in the
data.

## Examples

``` r
data(solubility_test)
str(solubility_test)
#> 'data.frame':    316 obs. of  2 variables:
#>  $ solubility: num  0.93 0.85 0.81 0.74 0.61 0.58 0.57 0.56 0.52 0.45 ...
#>  $ prediction: num  0.368 -0.15 -0.505 0.54 -0.479 ...
```
