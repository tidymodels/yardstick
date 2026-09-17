# errors with class_pred input

    Code
      brier_class_vec(cp_truth, estimate)
    Condition
      Error in `brier_class_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      brier_class_vec(1, 1, na_rm = "yes")
    Condition
      Error in `brier_class_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

