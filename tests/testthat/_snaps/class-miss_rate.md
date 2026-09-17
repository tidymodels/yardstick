# work with class_pred input

    Code
      miss_rate_vec(cp_truth, cp_estimate)
    Condition
      Error in `miss_rate_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      miss_rate_vec(1, 1, na_rm = "yes")
    Condition
      Error in `miss_rate_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# Binary returns `NA` with a warning when results are undefined (#98)

    Code
      out <- miss_rate_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `miss_rate()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Miss rate is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level, a

# Multiclass returns averaged value with warning when results undefined (#98)

    Code
      out <- miss_rate_vec(truth, estimate)
    Condition
      Warning:
      While computing multiclass `miss_rate()`, some levels had no true events (i.e. `true_positive + false_negative = 0`).
      Miss rate is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted events actually occurred for each problematic event level:
      'b': 0, 'c': 1

