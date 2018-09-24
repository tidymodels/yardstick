# yardstick 0.0.2.9000

## New features

* All metrics now have a data frame version and a vector version.
* The data frame version now returns a tibble rather than a numeric. This is
a breaking change. Use the vector versions for the old behavior.
* Data frame metric functions now work on grouped data frames and produce
1 row per group.
* There is now a `grouped_df` method for `conf_mat()` that returns a tibble
with a list column of `conf_mat` objects.

## Other changes

* `roc_auc()`, `pr_auc()`, and `roc_curve()` now have an `estimate` parameter
in place of using `...` to specify the class probability column.
* `broom` has been moved from `Depends` to `Suggests`.
* Summary methods for `conf_mat()` now return a tibble consistent with other
metric functions.

# yardstick 0.0.2

* Unweighted Kappa (via `kap`) is availible and is also returned by `metrics`. 
* Detection prevalence and balanced accuracy were added. 
* `roc_curve` is a tidy method for getting the points on an ROC curve. 
* Mean absolute error was added to `metrics` for regression data sets. 
* Mean absolute percent error (`mape`) was added. 


# `yardstick` 0.0.1

* First CRAN release
