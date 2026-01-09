# errors with class_pred input

    Code
      pr_auc_vec(cp_truth, estimate)
    Condition
      Error in `pr_auc_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      pr_auc_vec(1, 1, na_rm = "yes")
    Condition
      Error in `pr_auc_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

