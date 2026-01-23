# Changelog

## yardstick (development version)

- [`get_metrics()`](https://yardstick.tidymodels.org/dev/reference/get_metrics.md)
  was added to return a
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  containing all metrics of a specified type.
  ([\#534](https://github.com/tidymodels/yardstick/issues/534))

- All class metrics and probability metrics now include mathematical
  formulas in their documentation.
  ([\#605](https://github.com/tidymodels/yardstick/issues/605))

- [`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md)
  documentation now includes the formula and clarifies the
  interpretation of positive and negative values.
  ([\#345](https://github.com/tidymodels/yardstick/issues/345))

- [`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md)
  documentation now correctly refers to the `cost` column of the
  data.frame that can be passed to the `costs` arguemtn.
  ([\#343](https://github.com/tidymodels/yardstick/issues/343))

- `new_metric()` and related functions gain an optional `range` argument
  to store the valid output range of a metric. This is a
  developer-facing change.
  ([\#572](https://github.com/tidymodels/yardstick/issues/572))

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  now provides a more informative error message when `estimate` is not
  explicitly named for class/prob or survival metric sets.
  ([\#504](https://github.com/tidymodels/yardstick/issues/504))

- All metrics now have documented ranges of possible values in addition
  to what direction is the best.
  ([\#572](https://github.com/tidymodels/yardstick/issues/572))

- The ranked probability score for ordinal classification data was added
  with
  [`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md).
  ([\#524](https://github.com/tidymodels/yardstick/issues/524))

- [`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md)
  has been enhanced to handle 0 valued estimates, no longer returning
  `Inf` or `NaN`.
  ([\#513](https://github.com/tidymodels/yardstick/issues/513))

- Fixed bug where ranked probability metrics didn’t work in combination
  with other classification metrics in
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md).
  ([\#539](https://github.com/tidymodels/yardstick/issues/539))

- Added infrastructure for survival metrics on the linear predictor.
  ([\#551](https://github.com/tidymodels/yardstick/issues/551))

- Added infrastructure for quantile metrics.
  ([\#569](https://github.com/tidymodels/yardstick/issues/569))

- Added quantile metric
  [`weighted_interval_score()`](https://yardstick.tidymodels.org/dev/reference/weighted_interval_score.md).
  ([\#569](https://github.com/tidymodels/yardstick/issues/569))

- Added checks to all metrics for `na_rm` argument.
  ([\#349](https://github.com/tidymodels/yardstick/issues/349))

- Removed crayon as a suggested package.
  ([\#574](https://github.com/tidymodels/yardstick/issues/574))

- Added improved argument checking for metrics with additional
  arguments.
  ([\#519](https://github.com/tidymodels/yardstick/issues/519))

- Fixed documentation to show equations correctly.
  ([\#541](https://github.com/tidymodels/yardstick/issues/541))

- [`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md)
  and
  [`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md)
  have been added to compute the false positive rate and false negative
  rate respectively
  ([\#336](https://github.com/tidymodels/yardstick/issues/336)).

## yardstick 1.3.2

CRAN release: 2025-01-22

- All messages, warnings and errors has been translated to use {cli}
  package ([\#517](https://github.com/tidymodels/yardstick/issues/517),
  [\#522](https://github.com/tidymodels/yardstick/issues/522)).

## yardstick 1.3.1

CRAN release: 2024-03-21

### Bug Fixes

- Bug was fixed in
  [`roc_curve_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_curve_survival.md)
  where wrong weights were used.
  ([\#495](https://github.com/tidymodels/yardstick/issues/495),
  [@asb2111](https://github.com/asb2111)).

- Output of
  [`roc_curve_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_curve_survival.md)
  now returns columns in same order as
  [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md).
  ([\#498](https://github.com/tidymodels/yardstick/issues/498))

## yardstick 1.3.0

CRAN release: 2024-01-19

### New Metrics

- The Brier score for survival data was added with
  [`brier_survival()`](https://yardstick.tidymodels.org/dev/reference/brier_survival.md).

- The Integrated Brier score for survival data was added with
  [`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md).

- The Concordance index for survival data was added with
  [`concordance_survival()`](https://yardstick.tidymodels.org/dev/reference/concordance_survival.md).

- Time-Dependent ROC curves estimation for right-censored data can now
  be calculated with
  [`roc_curve_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_curve_survival.md).

- Time-Dependent ROC AUC estimation for right-censored data can now be
  calculated with
  [`roc_auc_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_auc_survival.md).

### Improvements

- [`demographic_parity()`](https://yardstick.tidymodels.org/dev/reference/demographic_parity.md),
  [`equalized_odds()`](https://yardstick.tidymodels.org/dev/reference/equalized_odds.md),
  and
  [`equal_opportunity()`](https://yardstick.tidymodels.org/dev/reference/equal_opportunity.md)
  are new metrics for measuring model fairness. Each is implemented with
  the
  [`new_groupwise_metric()`](https://yardstick.tidymodels.org/dev/reference/new_groupwise_metric.md)
  constructor, a general interface for defining group-aware metrics that
  allows for quickly and flexibly defining fairness metrics with the
  problem context in mind.

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  can now be used with a combination of dynamic and static survival
  metrics.

- Added a print method for metrics and metric sets
  ([\#435](https://github.com/tidymodels/yardstick/issues/435)).

- All warnings and errors have been updated to use the cli package for
  increased clarity and consistency.
  ([\#456](https://github.com/tidymodels/yardstick/issues/456),
  [\#457](https://github.com/tidymodels/yardstick/issues/457),
  [\#458](https://github.com/tidymodels/yardstick/issues/458))

- [`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md)
  now throws an error if input data only includes 1 evalution time
  point. ([\#460](https://github.com/tidymodels/yardstick/issues/460))

- Clarifying documentation about how `event_level` always default to
  `"first`.
  ([\#432](https://github.com/tidymodels/yardstick/issues/432))

### Bug Fixes

- Metrics now throw more informative error if `estimate` argument is
  wrongly used.
  ([\#443](https://github.com/tidymodels/yardstick/issues/443))

### Breaking Changes

- Curve metrics now throw an informative error instead of returning `NA`
  when missing values are found and `na_rm = FALSE`.
  ([\#344](https://github.com/tidymodels/yardstick/issues/344))

## yardstick 1.2.0

CRAN release: 2023-04-21

### New Metrics

- The Brier score for classification was added with
  [`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md)
  ([\#139](https://github.com/tidymodels/yardstick/issues/139)).

### Improvements

- The global option, `yardstick.event_first`, has been hard deprecated
  in favor of using explicit argument, `event_level`. Setting this
  option will now produce an warning, but won’t have any effect.
  ([\#173](https://github.com/tidymodels/yardstick/issues/173))

- Removed start-up message about `event_level` argument.

- yardstick metric functions now use a pure tidyselect interface for
  `truth`, `estimate`, and the `...` of class probability metrics
  ([\#322](https://github.com/tidymodels/yardstick/issues/322)).

- Changed the default aspect ratio for PR curves to be 1.0.

- Error messages now show what user-facing function was called
  ([\#348](https://github.com/tidymodels/yardstick/issues/348)).

- classification and probability metrics now fully support `class_pred`
  objects from {probably} package
  ([\#341](https://github.com/tidymodels/yardstick/issues/341)).

### Bug Fixes

- Using
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  on a metric created with
  [`metric_tweak()`](https://yardstick.tidymodels.org/dev/reference/metric_tweak.md)
  will no longer produces an error, and will favor arguments set with
  [`metric_tweak()`](https://yardstick.tidymodels.org/dev/reference/metric_tweak.md)
  ([\#351](https://github.com/tidymodels/yardstick/issues/351)).

- Metric summarizers no longer error if column names in `data` conflict
  with argument names
  ([\#382](https://github.com/tidymodels/yardstick/issues/382)).

- [`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md)
  no longer throw errors listed as internal
  ([\#327](https://github.com/tidymodels/yardstick/issues/327)).

### Developer

- [`metric_vec_template()`](https://yardstick.tidymodels.org/dev/reference/metric_vec_template.md)
  is being soft deprecated in favor of a more manual and flexible metric
  creation approach.
  [`yardstick_remove_missing()`](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
  and
  [`yardstick_any_missing()`](https://yardstick.tidymodels.org/dev/reference/yardstick_remove_missing.md)
  have been added for treatment of missing values.
  [`check_class_metric()`](https://yardstick.tidymodels.org/dev/reference/check_metric.md),
  [`check_numeric_metric()`](https://yardstick.tidymodels.org/dev/reference/check_metric.md),
  and
  [`check_prob_metric()`](https://yardstick.tidymodels.org/dev/reference/check_metric.md)
  have been added to perform standardized input checking for
  classification, regression and class probability metrics respectively.
  These changes mean that it is the developer’s responsibility to
  perform validation of `truth` and `estimate` input.
  ([\#337](https://github.com/tidymodels/yardstick/issues/337)).

- [`metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric_summarizer.md)
  is being soft deprecated in favor of the more specific newly added
  [`class_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
  [`numeric_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
  [`prob_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
  and
  [`curve_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
  ([\#322](https://github.com/tidymodels/yardstick/issues/322)).

- [`dots_to_estimate()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md)
  is being soft deprecated along with
  [`metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric_summarizer.md).
  [`dots_to_estimate()`](https://yardstick.tidymodels.org/dev/reference/developer-helpers.md)
  is not needed with
  [`prob_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md),
  and
  [`curve_metric_summarizer()`](https://yardstick.tidymodels.org/dev/reference/metric-summarizers.md)
  ([\#329](https://github.com/tidymodels/yardstick/issues/329)).

## yardstick 1.1.0

CRAN release: 2022-09-07

- Emil Hvitfeldt is now the maintainer
  ([\#315](https://github.com/tidymodels/yardstick/issues/315)).

- Improved on the chained error thrown by
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  when one of the metric computations fails
  ([\#313](https://github.com/tidymodels/yardstick/issues/313)).

## yardstick 1.0.0

CRAN release: 2022-06-06

- All yardstick metrics now support case weights through the new
  `case_weights` argument. This also includes metric-adjacent functions
  like
  [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md),
  [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md),
  [`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md),
  and
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md).

- The `options` argument of
  [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md),
  [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md),
  [`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md),
  [`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md),
  and
  [`metrics()`](https://yardstick.tidymodels.org/dev/reference/metrics.md)
  that was passed along to the pROC package is now deprecated and no
  longer has any affect. This is a result of changing to an ROC curve
  implementation that supports case weights, but does not support any of
  the previous options. If you need these options, we suggest wrapping
  pROC yourself in a custom metric
  ([\#296](https://github.com/tidymodels/yardstick/issues/296)).

- [`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md)
  now ignores any inputs passed through `...` and warns if you try to do
  such a thing. Previously, those were passed on to
  [`base::table()`](https://rdrr.io/r/base/table.html), but with the
  addition of case weight support,
  [`table()`](https://rdrr.io/r/base/table.html) is no longer used
  ([\#295](https://github.com/tidymodels/yardstick/issues/295)).

- Fixed a small mistake in
  [`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md) where
  the unbiased covariance wasn’t being used when `bias = FALSE`.

- [`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md)
  now throws a more correct warning if `0` is in the denominator when
  computing
  [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md)
  internally. Additionally, in the multiclass case it now removes the
  levels where this occurs from the multiclass weighted average
  computation, which is consistent with how other metrics were updated
  to handle this in
  [\#118](https://github.com/tidymodels/yardstick/issues/118)
  ([\#265](https://github.com/tidymodels/yardstick/issues/265)).

- Improved on some possible ambiguity in the documentation of the `data`
  argument for all metrics
  ([\#255](https://github.com/tidymodels/yardstick/issues/255)).

- purrr has been removed from Suggests.

- The pROC package has been removed as a dependency
  ([\#300](https://github.com/tidymodels/yardstick/issues/300)).

- Moved the Custom Metrics vignette to tidymodels.org
  ([\#236](https://github.com/tidymodels/yardstick/issues/236)).

## yardstick 0.0.9

CRAN release: 2021-11-22

- New metric
  [`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md)
  was added
  ([\#146](https://github.com/tidymodels/yardstick/issues/146)).

- [`sensitivity()`](https://yardstick.tidymodels.org/dev/reference/sens.md)
  and
  [`specificity()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
  now work correctly with the tune and workflowsets packages
  ([\#232](https://github.com/tidymodels/yardstick/issues/232)).

- [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md)
  now throws a more informative error if `truth` doesn’t have any
  control or event observations.

- dplyr 1.0.0 is now required. This allowed us to remove multiple usages
  of [`dplyr::do()`](https://dplyr.tidyverse.org/reference/do.html) in
  favor of
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  which can now return packed data frame columns and multiple rows per
  group.

- Removed internal hardcoding of `"dplyr_error"` to avoid issues with an
  upcoming dplyr 1.0.8 release
  ([\#244](https://github.com/tidymodels/yardstick/issues/244)).

- Updated test suite to testthat 3e
  ([\#243](https://github.com/tidymodels/yardstick/issues/243)).

- Internal upkeep has been done to move from `rlang::warn(.subclass = )`
  to `rlang::warn(class = )`, since the `.subclass` argument has been
  deprecated
  ([\#225](https://github.com/tidymodels/yardstick/issues/225)).

## yardstick 0.0.8

CRAN release: 2021-03-28

- New
  [`metric_tweak()`](https://yardstick.tidymodels.org/dev/reference/metric_tweak.md)
  for adjusting the default values of optional arguments in an existing
  yardstick metric. This is useful to quickly adjust the defaults of a
  metric that will be included in a
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md),
  especially if that metric set is going to be used for tuning with the
  tune package
  ([\#206](https://github.com/tidymodels/yardstick/issues/206),
  [\#182](https://github.com/tidymodels/yardstick/issues/182)).

- New
  [`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md)
  metric for computing the cost of a poor class probability prediction
  using user-defined costs
  ([\#3](https://github.com/tidymodels/yardstick/issues/3)).

- New [`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md)
  for computing the mean signed deviation (also called mean signed
  difference, or mean signed error)
  ([\#183](https://github.com/tidymodels/yardstick/issues/183),
  [@datenzauberai](https://github.com/datenzauberai)).

- `class_pred` objects from the
  [probably](https://probably.tidymodels.org/) package are now
  supported, and are automatically converted to factors before computing
  any metric. Note that this means that any equivocal values are
  materialized as `NA`
  ([\#198](https://github.com/tidymodels/yardstick/issues/198)).

- The [`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md)
  metric has a new `weighting` argument to apply linear or quadratic
  weightings before computing the kappa value
  ([\#2](https://github.com/tidymodels/yardstick/issues/2),
  [\#125](https://github.com/tidymodels/yardstick/issues/125),
  [@jonthegeek](https://github.com/jonthegeek)).

- When
  [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) is
  undefined when computing
  [`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md),
  [`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md),
  [`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md),
  or
  [`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md),
  a sensitivity warning is now correctly thrown, rather than a recall
  warning ([\#101](https://github.com/tidymodels/yardstick/issues/101)).

- The
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method for gain curves now plots the curve line on top of the shaded
  polygon, resulting in a sharper look for the line itself
  ([\#192](https://github.com/tidymodels/yardstick/issues/192),
  [@eddjberry](https://github.com/eddjberry)).

- The
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for `conf_mat` now respect user-defined dimension names added
  through `conf_mat(dnn = )` or from converting a table with dimension
  names to a `conf_mat`
  ([\#191](https://github.com/tidymodels/yardstick/issues/191)).

- Added an
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  method for `metric_set` objects. Printing a `metric_set` now uses this
  to print out a tibble rather than a data frame
  ([\#186](https://github.com/tidymodels/yardstick/issues/186)).

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/yardstick/issues/204)
  ([\#204](https://github.com/tidymodels/yardstick/issues/204)).

## yardstick 0.0.7

CRAN release: 2020-07-13

- The global option, `yardstick.event_first`, has been deprecated in
  favor of the new explicit argument, `event_level`. All metric
  functions that previously supported changing the “event” level have
  gained this new argument. The global option was a historical design
  decision that can be classified as a case of a hidden argument.
  Existing code that relied on this global option will continue to work
  in this version of yardstick, however you will now get a
  once-per-session warning that requests that you update to instead use
  the explicit `event_level` argument. The global option will be
  completely removed in a future version. To update, follow the guide
  below ([\#163](https://github.com/tidymodels/yardstick/issues/163)).

      `options(yardstick.event_first = TRUE)`  -> `event_level = "first"` (the default)
      `options(yardstick.event_first = FALSE)` -> `event_level = "second"`

- The
  [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)
  Hand-Till multiclass estimator will now ignore levels in `truth` that
  occur zero times in the actual data. With other methods of multiclass
  averaging, this usually returns an `NA`, however, ignoring levels in
  this manner is more consistent with implementations in the
  HandTill2001 and pROC packages
  ([\#123](https://github.com/tidymodels/yardstick/issues/123)).

- [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)
  and
  [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md)
  now set `direction = "<"` when computing the ROC curve using
  `pROC::roc()`. Results were being computed incorrectly with
  `direction = "auto"` when most probability values were predicting the
  wrong class
  ([\#123](https://github.com/tidymodels/yardstick/issues/123)).

- [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)
  now respects the (deprecated) global option `yardstick.event_first`.
  However, you should instead change the relevant event level through
  the `event_level` argument.

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  now strips the package name when auto-labeling functions
  ([@rorynolan](https://github.com/rorynolan),
  [\#151](https://github.com/tidymodels/yardstick/issues/151)).

- There are three new helper functions for more easily creating custom
  metric functions:
  [`new_class_metric()`](https://yardstick.tidymodels.org/dev/reference/new-metric.md),
  [`new_prob_metric()`](https://yardstick.tidymodels.org/dev/reference/new-metric.md),
  and
  [`new_numeric_metric()`](https://yardstick.tidymodels.org/dev/reference/new-metric.md).

- Rcpp has been removed as a direct dependency.

## yardstick 0.0.6

CRAN release: 2020-03-17

- [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)
  now warns when there are no events or controls in the provided `truth`
  column, and returns `NA` ([@dpastling](https://github.com/dpastling),
  [\#132](https://github.com/tidymodels/yardstick/issues/132)).

- Adds
  [`sensitivity()`](https://yardstick.tidymodels.org/dev/reference/sens.md)
  and
  [`specificity()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
  as *aliases* for
  [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md) and
  [`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
  respectively, avoids conflict with other packages
  e.g. `readr::spec()`.

- [`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)
  and
  [`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md)
  are two new ROC AUC metrics for multiclass classifiers. These measure
  the AUC of each class against the rest,
  [`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)
  using the uniform class distribution
  ([\#69](https://github.com/tidymodels/yardstick/issues/69)) and
  [`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md)
  using the a priori class distribution
  ([\#70](https://github.com/tidymodels/yardstick/issues/70)).

## yardstick 0.0.5

CRAN release: 2020-01-23

### Other improvements

- The
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  heat map for confusion matrices now places the predicted values on the
  `x` axis and the truth values on the `y` axis to be more consistent
  with the confusion matrix
  [`print()`](https://rdrr.io/r/base/print.html) method.

- The
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  mosaic plot for confusion matrices had the `x` and `y` axis labels
  backwards. This has been corrected.

## yardstick 0.0.4

CRAN release: 2019-08-26

### New metrics and functionality

- [`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md) is a
  new numeric metric for computing the index of ideality of correlation.
  It can be seen as a potential alternative to the traditional
  correlation coefficient, and has been used in QSAR models
  ([@jyuu](https://github.com/jyuu),
  [\#115](https://github.com/tidymodels/yardstick/issues/115)).

- [`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md)
  is a new probability metric that can be used as an alternative to
  [`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md).
  It has the benefit of avoiding any issues of ambiguity in the case
  where `recall == 0` and the current number of false positives is `0`.

### Other improvements

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  output now includes a `metrics` attribute which contains a list of the
  original metric functions used to generate the metric set.

- Each metric function now has a `direction` attribute attached to it,
  specifying whether to minimize or maximize the metric.

- Classification metrics that can potentially have a `0` value
  denominator now throw an informative warning when this case occurs.
  These include
  [`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md),
  [`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md),
  [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md),
  and [`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)
  ([\#98](https://github.com/tidymodels/yardstick/issues/98)).

- The
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method for
  [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
  has been improved to always set the axis limits to `c(0, 1)`.

- All valid arguments to `pROC::roc()` are now utilized, including those
  passed on to `pROC::auc()`.

- Documentation for class probability metrics has been improved with
  more informative examples
  ([@rudeboybert](https://github.com/rudeboybert),
  [\#100](https://github.com/tidymodels/yardstick/issues/100)).

### Bug fixes

- [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)
  now uses the min/max rule before computing the log of the estimated
  probabilities to avoid problematic undefined log values
  ([\#103](https://github.com/tidymodels/yardstick/issues/103)).

- [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
  now places a `1` as the first precision value, rather than `NA`. While
  `NA` is technically correct as precision is undefined here, `1` is
  practically more correct because it generates a correct PR Curve graph
  and, more importantly, allows
  [`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md)
  to compute the correct AUC.

- [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
  could generate the wrong results in the somewhat rare case when two
  class probability estimates were the same, but had different truth
  values.

- [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
  (and subsequently
  [`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md))
  now generates the correct curve when there are duplicate class
  probability values (reported by
  [@dariyasydykova](https://github.com/dariyasydykova),
  [\#93](https://github.com/tidymodels/yardstick/issues/93)).

- Binary
  [`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md) now
  avoids integer overflow when the confusion matrix elements are large
  ([\#108](https://github.com/tidymodels/yardstick/issues/108)).

## yardstick 0.0.3

CRAN release: 2019-03-08

### New metrics and functionality

- [`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md) is
  a numeric metric for the mean absolute scaled error. It is generally
  useful when forecasting with time series
  ([@alexhallam](https://github.com/alexhallam),
  [\#68](https://github.com/tidymodels/yardstick/issues/68)).

- [`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md)
  is a numeric metric that is less sensitive to outliers than
  [`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md),
  but is more sensitive than
  [`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md) for
  small errors ([@blairj09](https://github.com/blairj09),
  [\#71](https://github.com/tidymodels/yardstick/issues/71)).

- [`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md)
  is a smoothed form of
  [`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md)
  ([@blairj09](https://github.com/blairj09),
  [\#71](https://github.com/tidymodels/yardstick/issues/71)).

- [`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)
  is a numeric metric that is based on percentage errors
  ([@riazhedayati](https://github.com/riazhedayati),
  [\#67](https://github.com/tidymodels/yardstick/issues/67)).

- `conf_mat` objects now have two
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for easy visualization of the confusion matrix as either a
  heat map or a mosaic plot
  ([@EmilHvitfeldt](https://github.com/EmilHvitfeldt),
  [\#10](https://github.com/tidymodels/yardstick/issues/10)).

### Other improvements

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  now returns a classed function. If numeric metrics are used, a
  `"numeric_metric_set"` function is returned. If class or probability
  metrics are used, a `"class_prob_metric_set"` is returned.

### Bug fixes

- Tests related to the fixed R 3.6
  [`sample()`](https://rdrr.io/r/base/sample.html) function have been
  fixed.

- [`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md)
  propagates `NA` values from
  [`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md)
  and
  [`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md)
  correctly ([\#77](https://github.com/tidymodels/yardstick/issues/77)).

- All `"micro"` estimators now propagate `NA` values through correctly.

- `roc_auc(estimator = "hand_till")` now correctly computes the metric
  when the column names of the probability matrix are not the exact same
  as the levels of `truth`. Note that the computation still assumes that
  the order of the supplied probability matrix columns still matches the
  order of `levels(truth)`, like other multiclass metrics
  ([\#86](https://github.com/tidymodels/yardstick/issues/86)).

## yardstick 0.0.2

CRAN release: 2018-11-05

### Breaking changes

A desire to standardize the yardstick API is what drove these breaking
changes. The output of each metric is now in line with tidy principles,
returning a tibble rather than a single numeric. Additionally, all
metrics now have a standard argument list so you should be able to
switch between metrics and combine them together effortlessly.

- All metrics now return a tibble rather than a single numeric value.
  This format allows metrics to work with grouped data frames (for
  resamples). It also allows you to bundle multiple metrics together
  with a new function,
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md).

- For all class probability metrics, now only 1 column can be passed to
  `...` when a binary implementation is used. Those metrics will no
  longer select only the first column when multiple columns are
  supplied, and will instead throw an error.

- The [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `conf_mat` objects now returns a tibble to be consistent with the
  change to the metric functions.

- For naming consistency, `mnLogLoss()` was renamed to
  [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)

- [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)
  now returns the **negative** log loss for the multinomial
  distribution.

- The argument `na.rm` has been changed to `na_rm` in all metrics to
  align with the `tidymodels` model implementation principles.

### Core features

- Each metric now has a vector interface to go alongside the data frame
  interface. All vector functions end in `_vec()`. The vector interface
  accepts vector/matrix inputs and returns a single numeric value.

- Multiclass support has been added for each classification metric. The
  support varies from one metric to the next, but generally macro and
  micro averaging is available for all metrics, with some metrics having
  specialized multiclass implementations (for example,
  [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)
  supports the multiclass generalization presented in a paper by Hand
  and Till). For more information, see
  [`vignette("multiclass", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/multiclass.md).

- All metrics now work with grouped data frames. This produces a tibble
  with as many rows as there are groups, and is useful when used
  alongside resampling techniques.

### New metrics and functionality

- [`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md)
  calculates the mean absolute percent error.

- [`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md) is a
  metric similar to
  [`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md)
  that calculates Cohen’s kappa.

- [`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md)
  calculates the number of predicted positive events relative to the
  total number of predictions.

- [`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md)
  calculates balanced accuracy as the average of sensitivity and
  specificity.

- [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md)
  calculates receiver operator curves and returns the results as a
  tibble.

- [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md)
  calculates precision recall curves.

- [`gain_curve()`](https://yardstick.tidymodels.org/dev/reference/gain_curve.md)
  and
  [`lift_curve()`](https://yardstick.tidymodels.org/dev/reference/lift_curve.md)
  calculate the information used in gain and lift curves.

- [`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md)
  is a measure of performance similar in spirit to AUC but applied to a
  gain curve.

- [`pr_curve()`](https://yardstick.tidymodels.org/dev/reference/pr_curve.md),
  [`roc_curve()`](https://yardstick.tidymodels.org/dev/reference/roc_curve.md),
  [`gain_curve()`](https://yardstick.tidymodels.org/dev/reference/gain_curve.md)
  and
  [`lift_curve()`](https://yardstick.tidymodels.org/dev/reference/lift_curve.md)
  all have
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for easy visualization.

- [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)
  constructs functions that calculate multiple metrics at once.

### Other improvements

- The infrastructure for creating metrics has been exposed to allow
  users to extend yardstick to work with their own metrics. You might
  want to do this if you want your metrics to work with grouped data
  frames out of the box, or if you want the standardization and error
  checking that yardstick already provides. See
  `vignette("custom-metrics", "yardstick")` for a few examples.

- A vignette describing the three classes of metrics used in yardstick
  has been added. It also includes a list of every metric available,
  grouped by class. See
  [`vignette("metric-types", "yardstick")`](https://yardstick.tidymodels.org/dev/articles/metric-types.md).

- The error messages in yardstick should now be much more informative,
  with better feedback about the types of input that each metric can use
  and about what kinds of metrics can be used together (i.e. in
  [`metric_set()`](https://yardstick.tidymodels.org/dev/reference/metric_set.md)).

- There is now a `grouped_df` method for
  [`conf_mat()`](https://yardstick.tidymodels.org/dev/reference/conf_mat.md)
  that returns a tibble with a list column of `conf_mat` objects.

- Each metric now has its own help page. This allows us to better
  document the nuances of each metric without cluttering the help pages
  of other metrics.

### Dependencies

- `broom` has been removed from Depends, and is replaced by `generics`
  in Suggests.

- `tidyr` and `ggplot2` have been moved to Suggests.

- `MLmetrics` has been removed as a dependency.

## yardstick 0.0.1

CRAN release: 2017-11-12

- First CRAN release
