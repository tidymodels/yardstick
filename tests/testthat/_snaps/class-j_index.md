# Binary `j_index()` returns `NA` with a warning when sensitivity is undefined (tp + fn = 0) (#265)

    Code
      out <- j_index_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `sens()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level, a

# Binary `j_index()` returns `NA` with a warning when specificity is undefined (tn + fp = 0) (#265)

    Code
      out <- j_index_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `spec()`, no true negatives were detected (i.e. `true_negative + false_positive = 0`).
      Specificity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted negatives(s) actually occurred for the problematic event level, a

# Multiclass `j_index()` returns averaged value with `NA`s removed + a warning when sensitivity is undefined (tp + fn = 0) (#265)

    Code
      out <- j_index_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `sens()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'c': 1

# Multiclass `j_index()` returns averaged value with `NA`s removed + a warning when specificity is undefined (tn + fp = 0) (#265)

    Code
      out <- j_index_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `sens()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'b': 1, 'c': 1
      Warning:
      While computing multiclass `spec()`, some levels had no true negatives (i.e. `true_negative + false_positive = 0`).
      Specificity is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted negatives actually occurred for each problematic event level:
      'a': 2

# work with class_pred input

    Code
      j_index_vec(cp_truth, cp_estimate)
    Condition
      Error in `j_index_vec()`:
      ! `truth` should not a <class_pred> object.

