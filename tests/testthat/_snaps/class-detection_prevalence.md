# work with class_pred input

    Code
      detection_prevalence_vec(cp_truth, cp_estimate)
    Condition
      Error in `detection_prevalence_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      detection_prevalence(1, 1, na_rm = "yes")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'detection_prevalence' applied to an object of class "c('double', 'numeric')"

