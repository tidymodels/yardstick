# work with class_pred input

    Code
      roc_dist_vec(cp_truth, cp_estimate)
    Condition
      Error in `roc_dist_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      roc_dist_vec(1, 1, na_rm = "yes")
    Condition
      Error in `roc_dist_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# Binary returns `NA` with a warning when results are undefined (#98)

    Code
      out <- roc_dist_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `sens()`, no true events were detected (i.e. `true_positive + false_negative = 0`).
      Sensitivity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted event(s) actually occurred for the problematic event level, a

---

    Code
      out <- roc_dist_vec(truth, estimate)
    Condition
      Warning:
      While computing binary `spec()`, no true negatives were detected (i.e. `true_negative + false_positive = 0`).
      Specificity is undefined in this case, and `NA` will be returned.
      Note that 1 predicted negatives(s) actually occurred for the problematic event level, a

