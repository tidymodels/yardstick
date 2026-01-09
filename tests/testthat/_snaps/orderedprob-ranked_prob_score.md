# errors with bad input

    Code
      ranked_prob_score_vec(cp_truth, estimate)
    Condition
      Error in `ranked_prob_score_vec()`:
      ! `truth` should not a <class_pred> object.

---

    Code
      ranked_prob_score_vec(two_class_example$truth, estimate)
    Condition
      Error in `ranked_prob_score_vec()`:
      ! `truth` should be a ordered factor, not a a <factor> object.

---

    Code
      ranked_prob_score_vec(ord_truth, estimate_1D)
    Condition
      Error in `ranked_prob_score_vec()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

# na_rm argument check

    Code
      ranked_prob_score_vec(1, 1, na_rm = "yes")
    Condition
      Error in `ranked_prob_score_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

