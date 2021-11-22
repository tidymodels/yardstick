# Binary `ppv()` returns `NA` with a warning when `sens()` is undefined (tp + fn = 0) (#101)

    Code
      out <- ppv_vec(truth, estimate)
    Warning <yardstick_warning_sens_undefined_binary>
      While computing binary `sens()`, no true events were detected (i.e. `true_positive + false_negative = 0`). 
      Sensitivity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occured for the problematic event level, 'a'.

