# `NA` values propagate from binary `precision()`

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

# `NA` values propagate from binary `recall()`

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

# Binary `f_meas()` returns `NA` with a warning when recall is undefined (tp + fn = 0) (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `recall()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level a

# Binary `f_meas()` returns `NA` with a warning when precision is undefined (tp + fp = 0) (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 1 true event(s) actually occurred for the problematic event level, a

# Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when recall is undefined (tp + fn = 0) (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `recall()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'c': 1

# Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when precision is undefined (tp + fn = 0) (#98)

    Code
      out <- f_meas_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of true events actually occurred for each problematic event level:
      'c': 1

# work with class_pred input

    Code
      f_meas_vec(cp_truth, cp_estimate)
    Condition
      Error in `f_meas_vec()`:
      ! `truth` should not a <class_pred> object.

