# roc_curve() - error is thrown when missing events

    Code
      roc_curve_vec(no_event$truth, no_event$Class1)[[".estimate"]]
    Condition
      Error in `stop_roc_truth_no_event()`:
      ! No event observations were detected in `truth` with event level 'Class1'.

# roc_curve() - error is thrown when missing controls

    Code
      roc_curve_vec(no_control$truth, no_control$Class1)[[".estimate"]]
    Condition
      Error in `stop_roc_truth_no_control()`:
      ! No control observations were detected in `truth` with control level 'Class2'.

# roc_curve() - multiclass one-vs-all approach results in error

    Code
      roc_curve_vec(no_event$obs, as.matrix(dplyr::select(no_event, VF:L)))[[
        ".estimate"]]
    Condition
      Error in `stop_roc_truth_no_control()`:
      ! No control observations were detected in `truth` with control level '..other'.

# roc_curve() - `options` is deprecated

    Code
      out <- roc_curve(two_class_example, truth, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_curve()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

