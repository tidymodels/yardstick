
<img src="yardstick_hex_thumb.png" align="center" height = "80px" align = "middle"/>

[![Build
Status](https://travis-ci.org/tidymodels/yardstick.svg?branch=master)](https://travis-ci.org/tidymodels/yardstick)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tidymodels/yardstick/master.svg)](https://codecov.io/github/tidymodels/yardstick?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/yardstick)](http://cran.rstudio.com/package=yardstick)
[![Downloads](http://cranlogs.r-pkg.org/badges/yardstick)](http://cran.rstudio.com/package=yardstick)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

`yardstick` is a package to estimate how well models are working using
[tidy data](https://www.jstatsoft.org/article/view/v059i10) principles.
The package webpage is <https://tidymodels.github.io/yardstick/> for
more information.

For example, suppose you create a classification model and predict a
data set. You might have data that look like this:

``` r
library(yardstick)
library(dplyr)

head(two_class_example)
```

    ##    truth  Class1   Class2 predicted
    ## 1 Class2 0.00359 0.996411    Class2
    ## 2 Class1 0.67862 0.321379    Class1
    ## 3 Class2 0.11089 0.889106    Class2
    ## 4 Class1 0.73516 0.264838    Class1
    ## 5 Class2 0.01624 0.983760    Class2
    ## 6 Class1 0.99928 0.000725    Class1

You can use a `dplyr`-like syntax to compute common performance
characteristics of the model and get them back in a data frame:

``` r
metrics(two_class_example, truth, predicted)
```

    ## # A tibble: 2 x 2
    ##   .metric  .estimate
    ##   <chr>        <dbl>
    ## 1 accuracy     0.838
    ## 2 kap          0.675

``` r
# or 

two_class_example %>% roc_auc(truth, Class1)
```

    ## # A tibble: 1 x 2
    ##   .metric .estimate
    ##   <chr>       <dbl>
    ## 1 roc_auc     0.939

[Quasiquotation](http://rlang.tidyverse.org/reference/quasiquotation.html)
can also be used:

``` r
# probability columns:
lvl <- levels(two_class_example$truth)

two_class_example %>% mnLogLoss(truth, !! lvl)
```

    ## # A tibble: 1 x 2
    ##   .metric   .estimate
    ##   <chr>         <dbl>
    ## 1 mnLogLoss    -0.328

## Installation

To install the package:

``` r
install.packages("yardstick")

## for development version:
require("devtools")
install_github("tidymodels/yardstick")
```
