# errors with class_pred input

    Code
      mn_log_loss_vec(cp_truth, estimate)
    Condition
      Error in `mn_log_loss_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      mn_log_loss_vec(1, 1, na_rm = "yes")
    Condition
      Error in `mn_log_loss_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# bad argument check

    Code
      mn_log_loss_vec(1, 1, sum = "yes")
    Condition
      Error in `mn_log_loss_vec()`:
      ! `sum` must be `TRUE` or `FALSE`, not the string "yes".

