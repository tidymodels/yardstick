# Liver Pathology Data

Liver Pathology Data

## Source

Altman, D.G., Bland, J.M. (1994) “Diagnostic tests 1: sensitivity and
specificity,” *British Medical Journal*, vol 308, 1552.

## Value

- pathology:

  a data frame

## Details

These data have the results of a *x*-ray examination to determine
whether liver is abnormal or not (in the `scan` column) versus the more
extensive pathology results that approximate the truth (in `pathology`).

## Examples

``` r
data(pathology)
str(pathology)
#> 'data.frame':    344 obs. of  2 variables:
#>  $ pathology: Factor w/ 2 levels "abnorm","norm": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ scan     : Factor w/ 2 levels "abnorm","norm": 1 1 1 1 1 1 1 1 1 1 ...
```
