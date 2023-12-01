# Binary `precision()` returns `NA` with a warning when undefined (tp + fp = 0) (#98)

    Code
      out <- precision_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and `NA` will be returned.
      Note that 1 true event(s) actually occurred for the problematic event level, a

# Multiclass `precision()` returns averaged value with `NA`s removed + a warning when undefined (tp + fp = 0) (#98)

    Code
      out <- precision_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of true events actually occurred for each problematic event level:
      'a': 1, 'b': 1, 'c': 1

---

    Code
      out <- precision_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`).
      Precision is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of true events actually occurred for each problematic event level:
      'c': 1

# work with class_pred input

    Code
      precision_vec(cp_truth, cp_estimate)
    Condition
      Error in `precision_vec()`:
      ! `truth` should not a <class_pred> object.

