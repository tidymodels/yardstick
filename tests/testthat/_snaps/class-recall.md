# Binary `recall()` returns `NA` with a warning when undefined (tp + fn = 0) (#98)

    Code
      out <- recall_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `recall()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level a

# Multiclass `recall()` returns averaged value with `NA`s removed + a warning when undefined (tp + fn = 0) (#98)

    Code
      out <- recall_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `recall()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Recall is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'b': 0, 'c': 1

# work with class_pred input

    Code
      recall_vec(cp_truth, cp_estimate)
    Condition
      Error in `recall_vec()`:
      ! `truth` should not a <class_pred> object.

