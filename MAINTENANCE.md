## Current state

yardstick has reached 1.0.0 status and is stable.
For the majority of users, there are 3 types of metrics, each of which has an internal class that is defined through `new_metric()`:

-   Numeric metrics

-   Class metrics

-   Class probability metrics

yardstick is a bit unique in that the actual functions it exports, like `accuracy()`, have extra classes and attributes attached to them.
This allows them to be used in `metric_set()`, which has to decide whether or not two metric functions are allowed to be combined in the same metric set or not.
For example, two numeric metric functions can be combined, but you can't combine a numeric metric with a class metric.
The only exception here is that you can combine a class metric with a class probability metric - in the resulting function you get back from `metric_set()`, the class metric will use the `estimate` interface and the class probability metric will use the `…` one.

For the current public facing API, I don't see any major changes needing to be made.
I'm fairly happy with how the 3 core metric classes work.
I think most of the work for yardstick could be done on improving the internal helpers (see below in Known issues), or with extending yardstick with new metric class types (see Future directions).
It is likely that the internal helpers will have to be improved first before you can add new metric class types, because they are quite complex as it is, making extending yardstick fairly difficult.

## Known issues

There is a very similar problem with `metric_vec_template()`.
It currently tries to handle validation and function calling for *all* of the different types of metrics.
This makes it extremely complex, hard to extend, and probably a bit brittle.
In particular `validate_truth_estimate_checks()` does some fairly complex S3 dispatch to perform its validation (kind of a home grown double dispatch on `truth` and `estimate`) which might be able to be rewritten in a cleaner way if we had separate `metric_vec_template()` functions for the different kinds of metric types.

There are a few issues where this high cognitive overhead comes into play, making it hard to add these features:

-   <https://github.com/tidymodels/yardstick/issues/311>

-   <https://github.com/tidymodels/yardstick/issues/305>

-   <https://github.com/tidymodels/yardstick/issues/251>

The complexity of `validate_truth_estimate_checks()` could be reduced by instead creating a few `check_*()` helpers that we force the metric writers to call themselves.
If we provide useful ones, then they would just call them in their `metric_vec()` function themselves and we'd avoid the double dispatch altogether because they'd be in charge of calling the correct `check_*()` function based on the type of `truth` and `estimate` that their metric works with.
Something like `check_factor_truth_factor_estimate(truth, estimate)`.
That would probably help with #305.

## Future directions

-   We'd like to officially hard deprecate support for `yardstick.event_first`, a global option that has been soft deprecated for awhile in favor of an explicit `event_level` argument.
    You should definitely run revdeps after removing it, but I would be fairly aggressive about getting rid of it at this point.
    You can probably remove it in the next minor release.
    <https://github.com/tidymodels/yardstick/issues/173>

-   A number of people seem interested in calibration metrics and calibration curves.
    This might be combined with the probably package in some way <https://github.com/tidymodels/yardstick/issues/150>.

-   Fairness metrics seem fairly popular and might fit in yardstick, but we don't have a clear sense of how they'd be implemented.
    <https://github.com/tidymodels/yardstick/issues/176>

-   We know that we want to add survival metrics to yardstick at some point.
    There is a POC pull request that adds some of the basic infrastructure, but it isn't fully fleshed out <https://github.com/tidymodels/yardstick/pull/222>.
    Adding this to yardstick is a bit challenging for a few reasons:

    -   What metrics do we actually care about adding?
        It isn't entirely clear if we have a list of metrics we really care about.
        I think it will be critical to scope our survival support narrowly, for example, the PR only supports right censoring right now.
        Trying to add everything at once will be hard.

    -   What does the API for this look like?
        Right now, there is a well-defined API for how yardstick functions work.
        Most of them take `truth` and `estimate` vectors, which are columns in a data frame.
        The probability metrics take `…` instead of a single `estimate` column when they need to supply multiple estimate columns.
        For survival metrics, things are a bit more complex because we have to consider the censorship aspect.
        The PR linked above requires that `truth` be a `Surv` object, which has the censorship baked into it.
        For the `estimate` column, the PR requires that it is a list-of tibbles (the same length as `truth`), i.e. the output of the `predict()` function in censored.
        This makes the API rather complex for normal usage, but I think the goal was to align nicely for usage within tidymodels.

    -   The metric in the PR is a dummy/naive roc auc curve for survival analysis.
        I imagine that other "real" survival metrics might need to take some kind of `time` argument, which would probably be a required argument, which might make the API a little tricky for use with tune and in `metric_set()`s.

    -   The metric in the PR also adds a new `.time` column to the output of the metric.
        No other yardstick functions have this.

    -   Because of the special nature of the function signature for survival metrics, and the extra `.time` column in the output, they probably can't be combined with non-survival metrics in `metric_set()`.
        For this reason, survival metric functions should get their own class, i.e. from `new_metric()`, and `metric_set()` should use only allow metrics with that class to be combined together.

    -   I think the `_vec()` variant in the PR is a little strange.
        All other `_vec()` variants return a single numeric value, but this one would return a data frame.
        It might just be how this function would have to work, but we might also consider not exporting a `_vec()` variant for survival metrics.

    -   The other thing to remember about adding new `truth` types like `Surv` is that the validation and estimator helpers will have to gain new S3 methods to handle it.
        For example `finalize_estimator_default()` and `validate_truth_estimate_types()` are internal generics that needed new S3 methods to handle `Surv` (they are in the PR).
