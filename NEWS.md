# Development version

## New metrics and functionality

* `huber_loss()` is a numeric metric that is less sensitive to outliers than
`rmse()`, but is more sensitive than `mae()` for small errors (@blairj09, #71).

# yardstick 0.0.2

## Breaking changes

A desire to standardize the yardstick API is what drove these breaking changes. The 
output of each metric is now in line with tidy principles, returning a tibble
rather than a single numeric. Additionally, all metrics now have a standard 
argument list so you should be able to switch between metrics
and combine them together effortlessly.

* All metrics now return a tibble rather than a single numeric value. This format
allows metrics to work with grouped data frames (for resamples). It also allows
you to bundle multiple metrics together with a new function, `metric_set()`.

* For all class probability metrics, now only 1 column can be passed to `...`
when a binary implementation is used. Those metrics will no longer select 
only the first column when multiple columns are supplied, and will instead
throw an error.

* The `summary()` method for `conf_mat` objects now returns a tibble
to be consistent with the change to the metric functions.

* For naming consistency, `mnLogLoss()` was renamed to `mn_log_loss()`

* `mn_log_loss()` now returns the **negative** log loss for the 
multinomial distribution. 

* The argument `na.rm` has been changed to `na_rm` in all metrics to align
with the `tidymodels` model implementation principles.

## Core features

* Each metric now has a vector interface to go alongside the data frame interface.
All vector functions end in `_vec()`. The vector interface accepts vector/matrix
inputs and returns a single numeric value.

* Multiclass support has been added for each classification metric. 
The support varies from one metric to the next, but generally macro and micro 
averaging is available for all metrics, with some metrics having specialized
multiclass implementations (for example, `roc_auc()` supports the 
multiclass generalization presented in a paper by Hand and Till). 
For more information, see `vignette("multiclass", "yardstick")`.

* All metrics now work with grouped data frames. This produces a tibble with
as many rows as there are groups, and is useful when used alongside resampling
techniques.

## New metrics and functionality

* `mape()` calculates the mean absolute percent error.

* `kap()` is a metric similar to `accuracy()` that calculates Cohen's kappa.

* `detection_prevalence()` calculates the number of predicted positive events
relative to the total number of predictions.

* `bal_accuracy()` calculates balanced accuracy as the average of sensitivity
and specificity.

* `roc_curve()` calculates receiver operator curves and returns the results as 
a tibble.

* `pr_curve()` calculates precision recall curves.

* `gain_curve()` and `lift_curve()` calculate the information used
in gain and lift curves.

* `gain_capture()` is a measure of performance similar in spirit to AUC
but applied to a gain curve.

* `pr_curve()`, `roc_curve()`, `gain_curve()` and `lift_curve()` all have 
`ggplot2::autoplot()` methods for easy visualization.

* `metric_set()` constructs functions that calculate 
multiple metrics at once.

## Other improvements

* The infrastructure for creating metrics has been exposed to allow
users to extend yardstick to work with their own metrics. You might want to
do this if you want your metrics to work with grouped data frames out of the
box, or if you want the standardization and error checking that yardstick 
already provides. See `vignette("custom-metrics", "yardstick")` for a few
examples.

* A vignette describing the three classes of metrics used in yardstick has been
added. It also includes a list of every metric available, grouped by class.
See `vignette("metric-types", "yardstick")`.

* The error messages in yardstick should now be much more informative, with
better feedback about the types of input that each metric can use and about
what kinds of metrics can be used together (i.e. in `metric_set()`).

* There is now a `grouped_df` method for `conf_mat()` that returns a tibble
with a list column of `conf_mat` objects.

* Each metric now has its own help page. This allows us to better document the
nuances of each metric without cluttering the help pages of other metrics.

## Dependencies

* `broom` has been removed from Depends, and is replaced by `generics`
in Suggests.

* `tidyr` and `ggplot2` have been moved to Suggests.

* `MLmetrics` has been removed as a dependency.


# yardstick 0.0.1

* First CRAN release
