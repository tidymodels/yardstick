# warning is thrown when missing events

    Code
      out <- roc_auc(no_event, truth, Class1)[[".estimate"]]
    Condition
      Warning:
      No event observations were detected in `truth` with event level 'Class1'.

# warning is thrown when missing controls

    Code
      out <- roc_auc(no_control, truth, Class1)[[".estimate"]]
    Condition
      Warning:
      No control observations were detected in `truth` with control level 'Class2'.

# multiclass one-vs-all approach results in multiple warnings

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Condition
      Warning:
      No event observations were detected in `truth` with event level 'Class1'.
      Warning:
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Condition
      Warning:
      No event observations were detected in `truth` with event level 'Class1'.
      Warning:
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Condition
      Warning:
      No event observations were detected in `truth` with event level 'Class1'.
      Warning:
      No control observations were detected in `truth` with control level '..other'.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Condition
      Warning:
      No event observations were detected in `truth` with event level 'Class1'.
      Warning:
      No control observations were detected in `truth` with control level '..other'.

# hand till approach throws warning and returns `NaN` when only 1 level has observations

    Code
      out <- roc_auc_vec(x, estimate, estimator = "hand_till")
    Condition
      Warning:
      No observations were detected in `truth` for level(s): 'y'
      Computation will proceed by ignoring those levels.

---

    Code
      out <- roc_auc_vec(x, estimate, estimator = "hand_till")
    Condition
      Warning:
      No observations were detected in `truth` for level(s): 'y', 'z'
      Computation will proceed by ignoring those levels.

# roc_auc() - `options` is deprecated

    Code
      out <- roc_auc(two_class_example, truth, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_auc()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

---

    Code
      out <- roc_auc_vec(truth = two_class_example$truth, estimate = two_class_example$
        Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_auc_vec()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

