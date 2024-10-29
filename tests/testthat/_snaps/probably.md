# `class_pred` can be converted to `factor` when computing metrics

    Code
      accuracy_vec(cp_truth, cp_estimate)
    Condition
      Error in `accuracy_vec()`:
      ! `truth` should not a <class_pred> object.

# `class_pred` errors when passed to `conf_mat()`

    Code
      conf_mat(cp_hpc_cv, obs, pred)
    Condition
      Error in `conf_mat()`:
      ! `truth` should not a <class_pred> object.

---

    Code
      conf_mat(dplyr::group_by(cp_hpc_cv, Resample), obs, pred)
    Condition
      Error in `conf_mat()`:
      ! `truth` should not a <class_pred> object.

# `class_pred` errors when passed to `metrics()`

    Code
      metrics(cp_df, truth, estimate, class1)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `accuracy()`.
      Caused by error:
      ! `truth` should not a <class_pred> object.

