# warning is thrown when missing events

    Code
      out <- roc_auc(no_event, truth, Class1)[[".estimate"]]
    Condition
      Warning:
      There was 1 warning in `dplyr::summarise()`.
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No event observations were detected in `truth` with event level 'Class1'.

# warning is thrown when missing controls

    Code
      out <- roc_auc(no_control, truth, Class1)[[".estimate"]]
    Condition
      Warning:
      There was 1 warning in `dplyr::summarise()`.
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No control observations were detected in `truth` with control level 'Class2'.

# multiclass one-vs-all approach results in multiple warnings

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Condition
      Warning:
      There were 2 warnings in `dplyr::summarise()`.
      The first warning was:
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No event observations were detected in `truth` with event level 'Class1'.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
        ".estimate"]]
    Condition
      Warning:
      There were 2 warnings in `dplyr::summarise()`.
      The first warning was:
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No event observations were detected in `truth` with event level 'Class1'.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Condition
      Warning:
      There were 2 warnings in `dplyr::summarise()`.
      The first warning was:
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No event observations were detected in `truth` with event level 'Class1'.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

---

    Code
      out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[
        ".estimate"]]
    Condition
      Warning:
      There were 2 warnings in `dplyr::summarise()`.
      The first warning was:
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! No event observations were detected in `truth` with event level 'Class1'.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

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

# can't use case weights and hand-till method

    Code
      roc_auc(hpc_cv, obs, VF:L, estimator = "hand_till", case_weights = weight)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `finalize_estimator_roc_auc()`:
      ! Can't specify both `estimator = 'hand_till'` and `case_weights`.

# roc_auc() - `options` is deprecated

    Code
      out <- roc_auc(two_class_example, truth, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_auc()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

---

    Code
      out <- roc_auc_vec(truth = two_class_example$truth, estimate = two_class_example$
        Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_auc_vec()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.
