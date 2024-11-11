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
      ! The ranked probability score requires the outcome to be an ordered factor, not a <factor> object.

---

    Code
      ranked_prob_score_vec(ord_truth, estimate_1D)
    Condition
      Error in `ranked_prob_score_vec()`:
      ! For these data, the ranked probability score requires `estimate` to have 2 probability columns.

