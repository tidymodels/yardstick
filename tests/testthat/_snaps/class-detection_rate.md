# work with class_pred input

    Code
      detection_rate_vec(probably::as_class_pred(truth, which = 1), estimate)
    Condition
      Error in `detection_rate_vec()`:
      ! `truth` should not a <class_pred> object.

# bad argument check

    Code
      detection_rate_vec(truth, estimate, na_rm = "yes")
    Condition
      Error in `detection_rate_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      detection_rate_vec(as.character(truth), estimate)
    Condition
      Error in `detection_rate_vec()`:
      ! `truth` should be a factor, not a a character vector.

---

    Code
      detection_rate_vec(truth, factor(c("a", "c"), levels = c("a", "c")))
    Condition
      Error in `detection_rate_vec()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: a and b.
      * `estimate`: a and c.

---

    Code
      detection_rate_vec(truth, estimate, case_weights = 1)
    Condition
      Error in `detection_rate_vec()`:
      ! `truth` (2) and `case_weights` (1) must be the same length.

---

    Code
      detection_rate_vec(truth, estimate, estimator = "unknown")
    Condition
      Error in `detection_rate_vec()`:
      ! `estimator` must be one of "binary", "macro", "micro", or "macro_weighted", not "unknown".

---

    Code
      detection_rate_vec(truth, estimate, event_level = "unknown")
    Condition
      Error in `validate_event_level()`:
      ! `event_level` must be "first" or "second".

---

    Code
      detection_rate(matrix(1, nrow = 2, ncol = 3))
    Condition
      Error in `detection_rate.table()`:
      ! `x` must have equal dimensions. `x` has 3 columns and 2 rows.

