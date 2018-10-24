# yardstick 0.0.2.9000

## New functions

* `pr_curve()` calculates precision recall curves.

* `metric_set()` constructs functions that calculate 
multiple metrics at once.

* `gain_curve()` and `lift_curve()` calculate the gain and lift.

* `gain_capture()` is a measure of performance similar in spirit to AUC
but applied to a gain curve.

* `pr_curve()`, `roc_curve()`, `gain_curve()` and `lift_curve()` now have 
`ggplot2::autoplot()` methods for easy visualization.

## Changes

### Breaking

* All metrics now have a data frame version and a vector 
version. The data frame version now returns a tibble rather than a numeric. Use 
the vector versions for the old behavior.

* `summary()` for `conf_mat` objects now returns a tibble,
consistent with other metric functions.

* For naming consistency, `mnLogLoss` was renamed to `mn_log_loss` and now 
returns the **negative** log loss for the multinomial distribution. 

### Features

* Data frame metric functions now work on grouped data frames and produce
1 row per group.

* There is now a `grouped_df` method for `conf_mat()` that returns a tibble
with a list column of `conf_mat` objects.

### Dependencies

* `broom` has been moved from `Depends` to `Suggests`.

* `tidyr` has been moved to `Suggests`.

* `MLmetrics` has been removed as a dependency.

# yardstick 0.0.2

* Unweighted Kappa (via `kap`) is availible and is also returned by `metrics`. 
* Detection prevalence and balanced accuracy were added. 
* `roc_curve` is a tidy method for getting the points on an ROC curve. 
* Mean absolute error was added to `metrics` for regression data sets. 
* Mean absolute percent error (`mape`) was added. 


# `yardstick` 0.0.1

* First CRAN release
