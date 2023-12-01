# Binary `ppv()` returns `NA` with a warning when `sens()` is undefined (tp + fn = 0) (#101)

    Code
      out <- ppv_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `sens()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level, a

# work with class_pred input

    Code
      ppv_vec(cp_truth, cp_estimate)
    Condition
      Error in `ppv_vec()`:
      ! `truth` should not a <class_pred> object.

