# warning is thrown when missing events

    Code
      out <- roc_auc(no_event, truth, Class1)[[".estimate"]]
    Warning <yardstick_warning_roc_truth_no_event>
      No event observations were detected in `truth` with event level 'Class1'.

# warning is thrown when missing controls

    Code
      out <- roc_auc(no_control, truth, Class1)[[".estimate"]]
    Warning <yardstick_warning_roc_truth_no_control>
      No control observations were detected in `truth` with control level 'Class2'.

# multiclass one-vs-all approach results in multiple warnings

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Warning <yardstick_warning_roc_truth_no_event>
      No event observations were detected in `truth` with event level 'Class1'.
    Warning <yardstick_warning_roc_truth_no_control>
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Warning <yardstick_warning_roc_truth_no_event>
      No event observations were detected in `truth` with event level 'Class1'.
    Warning <yardstick_warning_roc_truth_no_control>
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Warning <yardstick_warning_roc_truth_no_event>
      No event observations were detected in `truth` with event level 'Class1'.
    Warning <yardstick_warning_roc_truth_no_control>
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Warning <yardstick_warning_roc_truth_no_event>
      No event observations were detected in `truth` with event level 'Class1'.
    Warning <yardstick_warning_roc_truth_no_control>
      No control observations were detected in `truth` with control level '..other'.

# hand till approach throws warning and returns `NaN` when only 1 level has observations

    Code
      out <- roc_auc_vec(x, estimate, estimator = "hand_till")
    Warning <warning>
      No observations were detected in `truth` for level(s): 'y'
      Computation will proceed by ignoring those levels.

---

    Code
      out <- roc_auc_vec(x, estimate, estimator = "hand_till")
    Warning <warning>
      No observations were detected in `truth` for level(s): 'y', 'z'
      Computation will proceed by ignoring those levels.

