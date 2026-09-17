# na_rm = FALSE errors if missing values are present

    Code
      roc_curve_vec(df$truth, df$Class1, na_rm = FALSE)
    Condition
      Error in `roc_curve_vec()`:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

# errors with class_pred input

    Code
      roc_curve_vec(cp_truth, estimate)
    Condition
      Error in `roc_curve_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      roc_curve_vec(1, 1, na_rm = "yes")
    Condition
      Error in `roc_curve_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# roc_curve() - error is thrown when missing events

    Code
      roc_curve_vec(no_event$truth, no_event$Class1)[[".estimate"]]
    Condition
      Error in `roc_curve_vec()`:
      ! No event observations were detected in `truth` with event level 'Class1'.

# roc_curve() - error is thrown when missing controls

    Code
      roc_curve_vec(no_control$truth, no_control$Class1)[[".estimate"]]
    Condition
      Error in `roc_curve()`:
      ! No control observations were detected in `truth` with control level 'Class2'.

# roc_curve() - multiclass one-vs-all approach results in error

    Code
      roc_curve_vec(no_event$obs, as.matrix(dplyr::select(no_event, VF:L)))[[
        ".estimate"]]
    Condition
      Error in `roc_curve()`:
      ! No control observations were detected in `truth` with control level '..other'.

# roc_curve() - `options` is deprecated

    Code
      out <- roc_curve(two_class_example, truth, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_curve()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

# thresholds argument throws errors when wrongly specied

    Code
      roc_curve(two_class_example, truth, Class1, thresholds = TRUE)
    Condition
      Error in `roc_curve()`:
      ! `thresholds` must be a numeric vector, not `TRUE`.

---

    Code
      roc_curve(two_class_example, truth, Class1, thresholds = -4)
    Condition
      Error in `roc_curve()`:
      ! `thresholds` must only take values between 0 and 1.
      The following 1 index falls outside the range: 1.

---

    Code
      roc_curve(two_class_example, truth, Class1, thresholds = seq(-1, 2, by = 0.2))
    Condition
      Error in `roc_curve()`:
      ! `thresholds` must only take values between 0 and 1.
      The following 10 indices fall outside the range: 1, 2, 3, 4, 5, 12, 13, 14, 15, and 16.

