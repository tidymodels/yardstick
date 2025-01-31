# validate_numeric_truth_numeric_estimate errors as expected

    Code
      validate_numeric_truth_numeric_estimate("1", 1)
    Condition
      Error:
      ! `truth` should be a numeric vector, not a string.

---

    Code
      validate_numeric_truth_numeric_estimate(1, "1")
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a string.

---

    Code
      validate_numeric_truth_numeric_estimate(matrix(1), 1)
    Condition
      Error:
      ! `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      validate_numeric_truth_numeric_estimate(1, matrix(1))
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a numeric matrix.

---

    Code
      validate_numeric_truth_numeric_estimate(1:4, 1:5)
    Condition
      Error:
      ! `truth` (4) and `estimate` (5) must be the same length.

---

    Code
      validate_binary_estimator(factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimator = "binary")
    Condition
      Error:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# validate_factor_truth_factor_estimate errors as expected

    Code
      validate_factor_truth_factor_estimate("1", 1)
    Condition
      Error:
      ! `truth` should be a factor, not a a string.

---

    Code
      validate_factor_truth_factor_estimate(c("a", "b", "a"), factor(c("a", "a", "a"),
      levels = c("a", "b")))
    Condition
      Error:
      ! `truth` should be a factor, not a a character vector.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "a", "a"), levels = c("a",
        "b")), c("a", "b", "a"))
    Condition
      Error:
      ! `estimate` should be a factor, not a a character vector.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "a", "a"), levels = c("a",
        "b")), 1:3)
    Condition
      Error:
      ! `estimate` should be a factor, not a an integer vector.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), factor(c("a", "a", "a"), levels = c("a", "b", "c")))
    Condition
      Error:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: a and b.
      * `estimate`: a, b, and c.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), factor(c("a", "a", "a", "a"), levels = c("a", "b")))
    Condition
      Error:
      ! `truth` (3) and `estimate` (4) must be the same length.

# validate_factor_truth_matrix_estimate errors as expected for binary

    Code
      validate_factor_truth_matrix_estimate(c("a", "b", "a"), 1:3, estimator = "binary")
    Condition
      Error:
      ! `truth` should be a factor, not a a character vector.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), c("a", "b", "a"), estimator = "binary")
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a character vector.

---

    Code
      validate_factor_truth_matrix_estimate(factor(character(), levels = c("a", "b")),
      matrix(1:6, ncol = 2), estimator = "binary")
    Condition
      Error:
      ! You are using a binary metric but have passed multiple columns to `...`.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b", "c")), 1:3, estimator = "binary")
    Condition
      Error:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# validate_factor_truth_matrix_estimate errors as expected for non-binary

    Code
      validate_factor_truth_matrix_estimate(c("a", "b", "a"), matrix(1:6, ncol = 2),
      estimator = "non binary")
    Condition
      Error:
      ! `truth` should be a factor, not a a character vector.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), 1:3, estimator = "non binary")
    Condition
      Error:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), matrix(as.character(1:6), ncol = 2), estimator = "non binary")
    Condition
      Error:
      ! The columns supplied in `...` should be numerics, not <cls>.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), matrix(1:15, ncol = 5), estimator = "non binary")
    Condition
      Error:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (5).

# validate_ordered_truth_matrix_estimate errors as expected for binary

    Code
      validate_ordered_truth_matrix_estimate(c("a", "b", "a"), 1:3, estimator = "binary")
    Condition
      Error:
      ! `truth` should be a ordered factor, not a a character vector.

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(c("a", "b", "a"), levels = c("a",
        "b")), c("a", "b", "a"), estimator = "binary")
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a character vector.

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(character(), levels = c("a", "b")),
      matrix(1:6, ncol = 2), estimator = "binary")
    Condition
      Error:
      ! You are using a binary metric but have passed multiple columns to `...`.

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(c("a", "b", "a"), levels = c("a",
        "b", "c")), 1:3, estimator = "binary")
    Condition
      Error:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# validate_ordered_truth_matrix_estimate errors as expected for non-binary

    Code
      validate_ordered_truth_matrix_estimate(c("a", "b", "a"), matrix(1:6, ncol = 2),
      estimator = "non binary")
    Condition
      Error:
      ! `truth` should be a ordered factor, not a a character vector.

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(c("a", "b", "a"), levels = c("a",
        "b")), 1:3, estimator = "non binary")
    Condition
      Error:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(c("a", "b", "a"), levels = c("a",
        "b")), matrix(as.character(1:6), ncol = 2), estimator = "non binary")
    Condition
      Error:
      ! The columns supplied in `...` should be numerics, not <cls>.

---

    Code
      validate_ordered_truth_matrix_estimate(ordered(c("a", "b", "a"), levels = c("a",
        "b")), matrix(1:15, ncol = 5), estimator = "non binary")
    Condition
      Error:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (5).

# validate_surv_truth_numeric_estimate errors as expected

    Code
      validate_surv_truth_numeric_estimate("1", 1)
    Condition
      Error:
      ! `truth` should be a Surv object, not a string.

---

    Code
      validate_surv_truth_numeric_estimate(lung_surv$surv_obj, as.character(lung_surv$
        .pred_time))
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a character vector.

---

    Code
      validate_surv_truth_numeric_estimate(lung_surv$surv_obj[1:5, ], lung_surv$
        .pred_time)
    Condition
      Error:
      ! `truth` (5) and `estimate` (228) must be the same length.

# validate_surv_truth_list_estimate errors as expected

    Code
      validate_surv_truth_list_estimate("1", 1)
    Condition
      Error:
      ! `truth` should be a Surv object, not a a string.

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj, lung_surv$list)
    Condition
      Error:
      ! All elements of `estimate` should be data.frames.

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj, lung_surv$list2)
    Condition
      Error:
      ! All data.frames of `estimate` should include column names: (.eval_time), (.pred_survival), and (.weight_censored).

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj, lung_surv$list4)
    Condition
      Error:
      ! All data.frames of `estimate` should include column names: (.eval_time), (.pred_survival), and (.weight_censored).

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj, as.character(lung_surv$
        .pred_time))
    Condition
      Error:
      ! `estimate` should be a list, not a a character vector.

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj[1:5, ], lung_surv$
        .pred_time)
    Condition
      Error:
      ! `estimate` should be a list, not a a double vector.

---

    Code
      validate_surv_truth_list_estimate(lung_surv$surv_obj[1:5, ], lung_surv$.pred)
    Condition
      Error:
      ! `truth` (5) and `estimate` (228) must be the same length.

---

    Code
      validate_surv_truth_list_estimate(lung_surv_not_all_same$surv_obj,
      lung_surv_not_all_same$.pred)
    Condition
      Error:
      x All the .eval_time columns of `estimate` must be identical.
      i The folllowing index differed from the first: 5, 10, and 14.

---

    Code
      validate_surv_truth_list_estimate(lung_surv_neg$surv_obj, lung_surv_neg$.pred)
    Condition
      Error:
      x Negative values of .eval_time are not allowed.
      i The following negative values were found: -100.

---

    Code
      validate_surv_truth_list_estimate(lung_surv_na$surv_obj, lung_surv_na$.pred)
    Condition
      Error:
      x Missing values in .eval_time are not allowed.

---

    Code
      validate_surv_truth_list_estimate(lung_surv_inf$surv_obj, lung_surv_inf$.pred)
    Condition
      Error:
      x Infinite values of .eval_time are not allowed.

---

    Code
      validate_surv_truth_list_estimate(lung_surv_duplicate$surv_obj,
      lung_surv_duplicate$.pred)
    Condition
      Error:
      x Duplicate values of .eval_time are not allowed.

# validate_case_weights errors as expected

    Code
      validate_case_weights(1:10, 11)
    Condition
      Error:
      ! `truth` (11) and `case_weights` (10) must be the same length.

