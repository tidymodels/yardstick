# yardstick 1.3.1

## Bug Fixes

* Bug was fixed in `roc_curve_survival()` where wrong weights were used. (#495, @asb2111).

* Output of `roc_curve_survival()` now returns columns in same order as `roc_curve()`. (#498)

# yardstick 1.3.0

## New Metrics

* The Brier score for survival data was added with `brier_survival()`.

* The Integrated Brier score for survival data was added with `brier_survival_integrated()`.

* The Concordance index for survival data was added with `concordance_survival()`.

* Time-Dependent ROC curves estimation for right-censored data can now be
calculated with `roc_curve_survival()`.

* Time-Dependent ROC AUC estimation for right-censored data can now be
calculated with `roc_auc_survival()`.

## Improvements

* `demographic_parity()`, `equalized_odds()`, and `equal_opportunity()` are new metrics for measuring model fairness. Each is implemented with the `new_groupwise_metric()` constructor, a general interface for defining group-aware metrics that allows for quickly and flexibly defining fairness metrics with the problem context in mind.

* `metric_set()` can now be used with a combination of dynamic and static survival metrics.

* Added a print method for metrics and metric sets (#435).

* All warnings and errors have been updated to use the cli package for increased clarity and consistency. (#456, #457, #458)

* `brier_survival_integrated()` now throws an error if input data only includes 1 evalution time point. (#460)

* Clarifying documentation about how `event_level` always default to `"first`. (#432)

## Bug Fixes

* Metrics now throw more informative error if `estimate` argument is wrongly used. (#443)

## Breaking Changes

* Curve metrics now throw an informative error instead of returning `NA` when missing values are found and `na_rm = FALSE`. (#344)

# yardstick 1.2.0

## New Metrics

* The Brier score for classification was added with `brier_class()` (#139).

## Improvements

* The global option, `yardstick.event_first`, has been hard deprecated in favor
  of using explicit argument, `event_level`. Setting this option will now 
  produce an warning, but won't have any effect. (#173)

* Removed start-up message about `event_level` argument.

* yardstick metric functions now use a pure tidyselect interface for `truth`, 
  `estimate`, and the `...` of class probability metrics (#322).

* Changed the default aspect ratio for PR curves to be 1.0. 

* Error messages now show what user-facing function was called (#348). 

* classification and probability metrics now fully support `class_pred` objects 
  from {probably} package (#341).

## Bug Fixes

* Using `metric_set()` on a metric created with `metric_tweak()` will no longer
  produces an error, and will favor arguments set with `metric_tweak()` (#351).
  
* Metric summarizers no longer error if column names in `data` conflict with
  argument names (#382).
  
* `conf_mat()` no longer throw errors listed as internal (#327).

## Developer

* `metric_vec_template()` is being soft deprecated in favor of a more manual
  and flexible metric creation approach. `yardstick_remove_missing()` and 
  `yardstick_any_missing()` have been added for treatment of missing values. 
  `check_class_metric()`, `check_numeric_metric()`,
  and `check_prob_metric()` have been added to perform standardized input
  checking for classification, regression and class probability metrics 
  respectively. These changes mean that it is the developer's responsibility 
  to perform validation of `truth` and `estimate` input. (#337).

* `metric_summarizer()` is being soft deprecated in favor of the more specific
  newly added `class_metric_summarizer()`, `numeric_metric_summarizer()`,
  `prob_metric_summarizer()`, and `curve_metric_summarizer()` (#322).
  
* `dots_to_estimate()` is being soft deprecated along with 
  `metric_summarizer()`. `dots_to_estimate()` is not needed with 
  `prob_metric_summarizer()`, and `curve_metric_summarizer()` (#329).
  
# yardstick 1.1.0

* Emil Hvitfeldt is now the maintainer (#315).

* Improved on the chained error thrown by `metric_set()` when one of the metric
  computations fails (#313).
  
# yardstick 1.0.0

* All yardstick metrics now support case weights through the new `case_weights`
  argument. This also includes metric-adjacent functions like `roc_curve()`,
  `pr_curve()`, `conf_mat()`, and `metric_set()`.
  
* The `options` argument of `roc_curve()`, `roc_auc()`, `roc_aunp()`,
  `roc_aunu()`, and `metrics()` that was passed along to the pROC package is
  now deprecated and no longer has any affect. This is a result of changing to
  an ROC curve implementation that supports case weights, but does not support
  any of the previous options. If you need these options, we suggest wrapping
  pROC yourself in a custom metric (#296).

* `conf_mat()` now ignores any inputs passed through `...` and warns if you
  try to do such a thing. Previously, those were passed on to `base::table()`,
  but with the addition of case weight support, `table()` is no longer used
  (#295).

* Fixed a small mistake in `ccc()` where the unbiased covariance wasn't being
  used when `bias = FALSE`.

* `j_index()` now throws a more correct warning if `0` is in the denominator
  when computing `sens()` internally. Additionally, in the multiclass case it
  now removes the levels where this occurs from the multiclass weighted average
  computation, which is consistent with how other metrics were updated to handle
  this in #118 (#265).

* Improved on some possible ambiguity in the documentation of the `data`
  argument for all metrics (#255).

* purrr has been removed from Suggests.

* The pROC package has been removed as a dependency (#300).

* Moved the Custom Metrics vignette to tidymodels.org (#236).

# yardstick 0.0.9

* New metric `poisson_log_loss()` was added (#146).

* `sensitivity()` and `specificity()` now work correctly with the tune and
  workflowsets packages (#232).
  
* `roc_curve()` now throws a more informative error if `truth` doesn't have any
  control or event observations.
  
* dplyr 1.0.0 is now required. This allowed us to remove multiple usages of
  `dplyr::do()` in favor of `dplyr::summarise()`, which can now return packed
  data frame columns and multiple rows per group.

* Removed internal hardcoding of `"dplyr_error"` to avoid issues with an
  upcoming dplyr 1.0.8 release (#244).

* Updated test suite to testthat 3e (#243).
  
* Internal upkeep has been done to move from `rlang::warn(.subclass = )` to
  `rlang::warn(class = )`, since the `.subclass` argument has been deprecated
  (#225).

# yardstick 0.0.8

* New `metric_tweak()` for adjusting the default values of optional arguments in
  an existing yardstick metric. This is useful to quickly adjust the defaults
  of a metric that will be included in a `metric_set()`, especially if that
  metric set is going to be used for tuning with the tune package (#206, #182).

* New `classification_cost()` metric for computing the cost of a poor class
  probability prediction using user-defined costs (#3).

* New `msd()` for computing the mean signed deviation (also called mean
  signed difference, or mean signed error) (#183, @datenzauberai).
  
* `class_pred` objects from the [probably](https://probably.tidymodels.org/)
  package are now supported, and are automatically converted to factors before
  computing any metric. Note that this means that any equivocal values are
  materialized as `NA` (#198).
  
* The `kap()` metric has a new `weighting` argument to apply linear or
  quadratic weightings before computing the kappa value (#2, #125, @jonthegeek).

* When `sens()` is undefined when computing `ppv()`, `npv()`, `j_index()`, or 
  `bal_accuracy()`, a sensitivity warning is now correctly thrown, rather than
  a recall warning (#101).

* The `autoplot()` method for gain curves now plots the curve line
  on top of the shaded polygon, resulting in a sharper look for the
  line itself (#192, @eddjberry).
  
* The `autoplot()` methods for `conf_mat` now respect user-defined dimension
  names added through `conf_mat(dnn = )` or from converting a table with
  dimension names to a `conf_mat` (#191).

* Added an `as_tibble()` method for `metric_set` objects. Printing a
  `metric_set` now uses this to print out a tibble rather than a data frame
  (#186).

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders
  here](https://github.com/tidymodels/yardstick/issues/204) (#204).

# yardstick 0.0.7

* The global option, `yardstick.event_first`, has been deprecated in favor of
  the new explicit argument, `event_level`. All metric functions that previously
  supported changing the "event" level have gained this new argument.
  The global option was a historical design decision that can be classified as
  a case of a hidden argument.
  Existing code that relied on this global option will continue to work in this
  version of yardstick, however you will now get a once-per-session warning
  that requests that you update to instead use the explicit `event_level`
  argument. The global option will be completely removed in a future version.
  To update, follow the guide below (#163).
  
  ```
  `options(yardstick.event_first = TRUE)`  -> `event_level = "first"` (the default)
  `options(yardstick.event_first = FALSE)` -> `event_level = "second"`
  ```

* The `roc_auc()` Hand-Till multiclass estimator will now ignore levels in
  `truth` that occur zero times in the actual data. With other methods of
  multiclass averaging, this usually returns an `NA`, however, ignoring
  levels in this manner is more consistent with implementations in the
  HandTill2001 and pROC packages (#123).

* `roc_auc()` and `roc_curve()` now set `direction = "<"` when computing the
  ROC curve using `pROC::roc()`. Results were being computed incorrectly with
  `direction = "auto"` when most probability values were predicting the wrong
  class (#123).

* `mn_log_loss()` now respects the (deprecated) global option
  `yardstick.event_first`. However, you should instead change the relevant
  event level through the `event_level` argument.

* `metric_set()` now strips the package name when auto-labeling functions
  (@rorynolan, #151).

* There are three new helper functions for more easily creating custom
  metric functions: `new_class_metric()`, `new_prob_metric()`, and
  `new_numeric_metric()`.

* Rcpp has been removed as a direct dependency.

# yardstick 0.0.6

* `roc_auc()` now warns when there are no events or controls in the provided `truth` column, and returns `NA` (@dpastling, #132).

* Adds `sensitivity()` and `specificity()` as _aliases_ for `sens()` and `spec()` respectively, avoids conflict with other packages e.g. `readr::spec()`.

* `roc_aunu()` and `roc_aunp()` are two new ROC AUC metrics for multiclass classifiers. These measure the AUC of each class against the rest, `roc_aunu()` using the uniform class distribution (#69) and `roc_aunp()` using the a priori class distribution (#70).

# yardstick 0.0.5

## Other improvements

* The `autoplot()` heat map for confusion matrices now places the predicted values on the `x` axis and the truth values on the `y` axis to be more consistent with the confusion matrix `print()` method.

* The `autoplot()` mosaic plot for confusion matrices had the `x` and `y` axis labels backwards. This has been corrected.

# yardstick 0.0.4

## New metrics and functionality

* `iic()` is a new numeric metric for computing the index of ideality of correlation. It can be seen as a potential alternative to the traditional correlation coefficient, and has been used in QSAR models (@jyuu, #115).

* `average_precision()` is a new probability metric that can be used as an alternative to `pr_auc()`. It has the benefit of avoiding any issues of ambiguity in the case where `recall == 0` and the current number of false positives is `0`.

## Other improvements

* `metric_set()` output now includes a `metrics` attribute which contains a list of the original metric functions used to generate the metric set.

* Each metric function now has a `direction` attribute attached to it, specifying whether to minimize or maximize the metric.

* Classification metrics that can potentially have a `0` value denominator now throw an informative warning when this case occurs. These include `recall()`, `precision()`, `sens()`, and `spec()` (#98).

* The `autoplot()` method for `pr_curve()` has been improved to always set the axis limits to `c(0, 1)`.

* All valid arguments to `pROC::roc()` are now utilized, including those passed on to `pROC::auc()`.

* Documentation for class probability metrics has been improved with more informative examples (@rudeboybert, #100).

## Bug fixes

* `mn_log_loss()` now uses the min/max rule before computing the log of the estimated probabilities to avoid problematic undefined log values (#103).

* `pr_curve()` now places a `1` as the first precision value, rather than `NA`. While `NA` is technically correct as precision is undefined here, `1` is practically more correct because it generates a correct PR Curve graph and, more importantly, allows `pr_auc()` to compute the correct AUC.

* `pr_curve()` could generate the wrong results in the somewhat rare case when two class probability estimates were the same, but had different truth values.

* `pr_curve()` (and subsequently `pr_auc()`) now generates the correct curve when there are duplicate class probability values (reported by @dariyasydykova, #93).

* Binary `mcc()` now avoids integer overflow when the confusion matrix elements are large (#108).

# yardstick 0.0.3

## New metrics and functionality

* `mase()` is a numeric metric for the mean absolute scaled error. It is 
generally useful when forecasting with time series (@alexhallam, #68).

* `huber_loss()` is a numeric metric that is less sensitive to outliers than
`rmse()`, but is more sensitive than `mae()` for small errors (@blairj09, #71).

* `huber_loss_pseudo()` is a smoothed form of `huber_loss()` (@blairj09, #71).

* `smape()` is a numeric metric that is based on percentage errors 
(@riazhedayati, #67).

* `conf_mat` objects now have two `ggplot2::autoplot()` methods for easy visualization
of the confusion matrix as either a heat map or a mosaic plot (@EmilHvitfeldt, #10).

## Other improvements

* `metric_set()` now returns a classed function. If numeric metrics are used,
a `"numeric_metric_set"` function is returned. If class or probability metrics
are used, a `"class_prob_metric_set"` is returned.

## Bug fixes

* Tests related to the fixed R 3.6 `sample()` function have been fixed.

* `f_meas()` propagates `NA` values from `precision()` and `recall()` correctly (#77).

* All `"micro"` estimators now propagate `NA` values through correctly.

* `roc_auc(estimator = "hand_till")` now correctly computes the metric when the column names of the probability matrix are not the exact same as the levels of `truth`. Note that the computation still assumes that the order of the supplied probability matrix columns still matches the order of `levels(truth)`, like other multiclass metrics (#86).

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
