# errors with class_pred input

    Code
      gain_capture_vec(cp_truth, estimate)
    Condition
      Error in `gain_capture_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      gain_capture_vec(1, 1, na_rm = "yes")
    Condition
      Error in `gain_capture_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

