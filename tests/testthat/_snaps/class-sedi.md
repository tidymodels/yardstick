# work with class_pred input

    Code
      sedi_vec(cp_truth, cp_estimate)
    Condition
      Error in `sedi_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      sedi_vec(1, 1, na_rm = "yes")
    Condition
      Error in `sedi_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

