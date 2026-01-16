# work with class_pred input

    Code
      f_meas_vec(cp_truth, cp_estimate)
    Condition
      Error in `f_meas_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      f_meas_vec(1, 1, na_rm = "yes")
    Condition
      Error in `f_meas_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# `NA` values propagate from binary `precision()` (#77)

    Code
      out <- precision_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 2 true event(s) actually occurred for the problematic event level, a
    Code
      expect <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 2 true event(s) actually occurred for the problematic event level, a

# `NA` values propagate from binary `recall()` (#77)

    Code
      out <- recall_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `recall()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and `NA` will be returned.
      Note that 2 predicted event(s) actually occurred for the problematic event level a
    Code
      expect <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `recall()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and `NA` will be returned.
      Note that 2 predicted event(s) actually occurred for the problematic event level a

# Binary returns `NA` with a warning when results are undefined (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `recall()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level a

---

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 1 true event(s) actually occurred for the problematic event level, a

# Multiclass returns averaged value a warning when results is undefined (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `recall()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'c': 1

# bad argument check

    Code
      f_meas_vec(1, 1, beta = "yes")
    Condition
      Error in `f_meas_vec()`:
      ! `beta` must be a number, not the string "yes".

