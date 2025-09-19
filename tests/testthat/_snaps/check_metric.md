# check_numeric_metric() validates case_weights

    Code
      check_numeric_metric(1:10, 1:10, 1:11)
    Condition
      Error:
      ! `truth` (10) and `case_weights` (11) must be the same length.

# check_numeric_metric() validates inputs

    Code
      check_numeric_metric(1, "1", 1)
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a string.

# check_class_metric() validates case_weights

    Code
      check_class_metric(letters, letters, 1:5)
    Condition
      Error:
      ! `truth` (26) and `case_weights` (5) must be the same length.

# check_class_metric() validates inputs

    Code
      check_class_metric(1, "1", 1)
    Condition
      Error:
      ! `truth` should be a factor, not a a number.

# check_class_metric() validates estimator

    Code
      check_class_metric(factor(c("a", "b", "a"), levels = c("a", "b", "c")), factor(
        c("a", "b", "a"), levels = c("a", "b", "c")), case_weights = 1:3, estimator = "binary")
    Condition
      Error:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# check_prob_metric() validates case_weights

    Code
      check_prob_metric(factor(c("a", "b", "a")), matrix(1:6, nrow = 2), 1:4,
      estimator = "binary")
    Condition
      Error:
      ! `truth` (3) and `case_weights` (4) must be the same length.

# check_prob_metric() validates inputs

    Code
      check_prob_metric(factor(c("a", "b", "a")), matrix(1:6, nrow = 2), 1:3,
      estimator = "binary")
    Condition
      Error:
      ! You are using a binary metric but have passed multiple columns to `...`.

# check_ordered_prob_metric() validates case_weights

    Code
      check_ordered_prob_metric(ordered(c("a", "b", "a")), matrix(1:6, nrow = 2), 1:4,
      estimator = "binary")
    Condition
      Error:
      ! `truth` (3) and `case_weights` (4) must be the same length.

# check_ordered_prob_metric() validates inputs

    Code
      check_ordered_prob_metric(ordered(c("a", "b", "a")), matrix(1:6, nrow = 2), 1:3,
      estimator = "binary")
    Condition
      Error:
      ! You are using a binary metric but have passed multiple columns to `...`.

# check_static_survival_metric() validates case_weights

    Code
      check_static_survival_metric(truth = lung_surv$surv_obj, estimate = lung_surv$
        .pred_survival, case_weights = 1:151)
    Condition
      Error:
      ! `truth` (228) and `case_weights` (151) must be the same length.

# check_static_survival_metric() validates inputs

    Code
      check_static_survival_metric(truth = lung_surv$surv_obj, estimate = as.character(
        lung_surv$inst), case_weights = 1:150)
    Condition
      Error:
      ! `truth` (228) and `case_weights` (150) must be the same length.

