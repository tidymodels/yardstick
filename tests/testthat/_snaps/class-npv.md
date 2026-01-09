# work with class_pred input

    Code
      accuracy_vec(cp_truth, cp_estimate)
    Condition
      Error in `accuracy_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      npv_vec(1, 1, na_rm = "yes")
    Condition
      Error in `npv_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

