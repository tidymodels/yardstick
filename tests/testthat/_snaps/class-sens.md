# Binary `sens()` returns `NA` with a warning when undefined (tp + fn = 0) (#98)

    Code
      out <- sens_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `sens()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level, a

# Multiclass `sens()` returns averaged value with `NA`s removed + a warning when undefined (tp + fn = 0) (#98)

    Code
      out <- sens_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `sens()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'b': 0, 'c': 1

# work with class_pred input

    Code
      sensitivity_vec(cp_truth, cp_estimate)
    Condition
      Error in `sensitivity_vec()`:
      ! `truth` should not a <class_pred> object.

