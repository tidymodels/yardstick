# check_numeric_metric() validates case_weights

    Code
      check_numeric_metric(1:10, 1:10, 1:11)
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (11) must have the same length as `truth` (10).

# check_numeric_metric() validates inputs

    Code
      check_numeric_metric(1, "1", 1)
    Condition
      Error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric, not a `character`.

# check_class_metric() validates case_weights

    Code
      check_class_metric(letters, letters, 1:5)
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (5) must have the same length as `truth` (26).

# check_class_metric() validates inputs

    Code
      check_class_metric(1, "1", 1)
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` should be a factor, not a `numeric`.

# check_class_metric() validates estimator

    Code
      check_class_metric(truth = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimate = factor(c("a", "b", "a"), levels = c("a", "b", "c")), case_weights = 1:
        3, estimator = "binary")
    Condition
      Error in `validate_binary_estimator()`:
      ! `estimator` is binary, only two class `truth` factors are allowed. A factor with 3 levels was provided.

# check_prob_metric() validates case_weights

    Code
      check_prob_metric(truth = factor(c("a", "b", "a")), estimate = matrix(1:6,
      nrow = 2), case_weights = 1:4, estimator = "binary")
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (4) must have the same length as `truth` (3).

# check_prob_metric() validates inputs

    Code
      check_prob_metric(truth = factor(c("a", "b", "a")), estimate = matrix(1:6,
      nrow = 2), case_weights = 1:3, estimator = "binary")
    Condition
      Error in `validate_factor_truth_matrix_estimate()`:
      ! You are using a binary metric but have passed multiple columns to `...`.

# check_dynamic_survival_metric() validates case_weights

    Code
      check_dynamic_survival_metric(truth = lung_surv$surv_obj, estimate = lung_surv$
        .pred, case_weights = 1:51, .time = 1:3)
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (51) must have the same length as `truth` (50).

# check_dynamic_survival_metric() validates inputs

    Code
      check_dynamic_survival_metric(truth = lung_surv$surv_obj, estimate = lung_surv$
        inst, case_weights = 1:50, .time = 1:3)
    Condition
      Error in `validate_surv_truth_list_estimate()`:
      ! `estimate` should be a list, not a `numeric`.

# check_static_survival_metric() validates case_weights

    Code
      check_static_survival_metric(truth = lung_surv$surv_obj, estimate = lung_surv$
        .pred, case_weights = 1:51)
    Condition
      Error in `validate_case_weights()`:
      ! `case_weights` (51) must have the same length as `truth` (50).

# check_static_survival_metric() validates inputs

    Code
      check_static_survival_metric(truth = lung_surv$surv_obj, estimate = as.character(
        lung_surv$inst), case_weights = 1:50)
    Condition
      Error in `validate_surv_truth_numeric_estimate()`:
      ! `estimate` should be a numeric, not a `character`.

