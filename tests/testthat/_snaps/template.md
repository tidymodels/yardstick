# case weights are validated

    Code
      accuracy_vec(truth, estimate, case_weights = 1)
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (1) must have the same length as `truth` (2).

---

    Code
      accuracy_vec(truth, estimate, case_weights = c("x", "y"))
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` must be an integer or double vector.

