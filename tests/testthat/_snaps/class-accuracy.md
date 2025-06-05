# work with class_pred input

    Code
      accuracy_vec(cp_truth, cp_estimate)
    Condition
      Error in `accuracy_vec()`:
      ! `truth` should not a <class_pred> object.

# reject character input

    Code
      accuracy_vec(two_class_example$truth == "Class1", c("TRUE", "FALSE")[as.integer(
        two_class_example$predicted)])
    Condition
      Error in `accuracy_vec()`:
      ! `estimate` should be a factor, not a a character vector.

---

    Code
      accuracy_vec(c("TRUE", "FALSE")[as.integer(two_class_example$truth)],
      two_class_example$predicted == "Class1")
    Condition
      Error in `accuracy_vec()`:
      ! `truth` should be a factor, not a a character vector.

