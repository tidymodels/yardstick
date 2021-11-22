# Binary `spec()` returns `NA` with a warning when undefined (tn + fp = 0) (#98)

    Code
      out <- spec_vec(truth, estimate)
    Warning <yardstick_warning_spec_undefined_binary>
      While computing binary `spec()`, no true negatives were detected (i.e. `true_negative + false_positive = 0`). 
      Specificity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted negatives(s) actually occured for the problematic event level, 'a'.

# Multiclass `spec()` returns averaged value with `NA`s removed + a warning when undefined (tn + fp = 0) (#98)

    Code
      out <- spec_vec(truth, estimate)
    Warning <yardstick_warning_spec_undefined_multiclass>
      While computing multiclass `spec()`, some levels had no true negatives (i.e. `true_negative + false_positive = 0`). 
      Specificity is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of predicted negatives actually occured for each problematic event level:
      'a': 2

