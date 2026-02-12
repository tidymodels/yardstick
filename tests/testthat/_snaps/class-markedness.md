# work with class_pred input

    Code
      markedness_vec(cp_truth, cp_estimate)
    Condition
      Error in `markedness_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      markedness_vec(1, 1, na_rm = "yes")
    Condition
      Error in `markedness_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# Binary returns `NA` with a warning when precision undefined (#98)

    Code
      out <- markedness_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 0 true event(s) actually occurred for the problematic event level, a

# Binary returns `NA` with a warning when inverse precision undefined (#98)

    Code
      out <- markedness_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `markedness()`, no predicted non-events were detected (i.e. `true_negative + false_negative = 0`).
      Markedness is undefined in this case, and `NA` will be returned.
      Note that 0 true non-event(s) actually occurred for the problematic event level, b

# Multiclass returns averaged value with warning when results undefined (#98)

    Code
      out <- markedness_vec(truth, estimate)

