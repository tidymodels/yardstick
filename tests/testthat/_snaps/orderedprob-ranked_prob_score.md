# errors with class_pred input

    Code
      ranked_prob_score_vec(cp_truth, estimate)
    Condition
      Error in `ranked_prob_score_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      ranked_prob_score_vec(1, 1, na_rm = "yes")
    Condition
      Error in `ranked_prob_score_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

