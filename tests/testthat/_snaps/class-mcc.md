# work with class_pred input

    Code
      mcc_vec(cp_truth, cp_estimate)
    Condition
      Error in `mcc_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      mcc_vec(1, 1, na_rm = "yes")
    Condition
      Error in `mcc_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

