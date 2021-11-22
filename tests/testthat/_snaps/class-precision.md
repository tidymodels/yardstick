# Binary `precision()` returns `NA` with a warning when undefined (tp + fp = 0) (#98)

    Code
      out <- precision_vec(truth, estimate)
    Warning <yardstick_warning_precision_undefined_binary>
      While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`). 
      Precision is undefined in this case, and `NA` will be returned.
      Note that 1 true event(s) actually occured for the problematic event level, 'a'.

# Multiclass `precision()` returns averaged value with `NA`s removed + a warning when undefined (tp + fp = 0) (#98)

    Code
      out <- precision_vec(truth, estimate)
    Warning <yardstick_warning_precision_undefined_multiclass>
      While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
      Precision is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of true events actually occured for each problematic event level:
      'a': 1
      'b': 1
      'c': 1

---

    Code
      out <- precision_vec(truth, estimate)
    Warning <yardstick_warning_precision_undefined_multiclass>
      While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
      Precision is undefined in this case, and those levels will be removed from the averaged result.
      Note that the following number of true events actually occured for each problematic event level:
      'c': 1

