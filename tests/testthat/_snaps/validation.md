# validate_numeric_truth_numeric_estimate errors as expected

    Code
      validate_numeric_truth_numeric_estimate("1", 1)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `truth` should be a numeric, not a `character`.

---

    Code
      validate_numeric_truth_numeric_estimate(1, "1")
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric, not a `character`.

---

    Code
      validate_numeric_truth_numeric_estimate(matrix(1), 1)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `truth` should be a numeric vector, not a numeric matrix.

---

    Code
      validate_numeric_truth_numeric_estimate(1, matrix(1))
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric vector, not a numeric matrix.

---

    Code
      validate_numeric_truth_numeric_estimate(1:4, 1:5)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! Length of `truth` (4) and `estimate` (5) must match.

---

    Code
      validate_binary_estimator(factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimator = "binary")
    Condition
      Error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# validate_factor_truth_factor_estimate errors as expected

    Code
      validate_factor_truth_factor_estimate("1", 1)
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` should be a factor, not a `character`.

---

    Code
      validate_factor_truth_factor_estimate(c("a", "b", "a"), factor(c("a", "a", "a"),
      levels = c("a", "b")))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` should be a factor, not a `character`.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "a", "a"), levels = c("a",
        "b")), c("a", "b", "a"))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `estimate` should be a factor, not a `character`.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "a", "a"), levels = c("a",
        "b")), 1:3)
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `estimate` should be a factor, not a `integer`.

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), factor(c("a", "a", "a"), levels = c("a", "b", "c")))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: a, b
      `estimate`: a, b, c

---

    Code
      validate_factor_truth_factor_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), factor(c("a", "a", "a", "a"), levels = c("a", "b")))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! Length of `truth` (3) and `estimate` (4) must match.

# validate_factor_truth_matrix_estimate errors as expected for binary

    Code
      validate_factor_truth_matrix_estimate(c("a", "b", "a"), 1:3, estimator = "binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! `truth` should be a factor, not a `character`.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), c("a", "b", "a"), estimator = "binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! `estimate` should be a numeric vector, not a `character` vector.

---

    Code
      validate_factor_truth_matrix_estimate(factor(character(), levels = c("a", "b")),
      matrix(1:6, ncol = 2), estimator = "binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b", "c")), 1:3, estimator = "binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# validate_factor_truth_matrix_estimate errors as expected for non-binary

    Code
      validate_factor_truth_matrix_estimate(c("a", "b", "a"), matrix(1:6, ncol = 2),
      estimator = "non binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! `truth` should be a factor, not a `character`.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), 1:3, estimator = "non binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), matrix(as.character(1:6), ncol = 2), estimator = "non binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! The columns supplied in `...` should be numerics, not `character`s.

---

    Code
      validate_factor_truth_matrix_estimate(factor(c("a", "b", "a"), levels = c("a",
        "b")), matrix(1:15, ncol = 5), estimator = "non binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (5).

