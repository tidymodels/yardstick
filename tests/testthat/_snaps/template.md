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

# numeric_metric_summarizer()'s errors when wrong things are passes

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = not_a_real_column_name,
        estimate = disp)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = mpg,
        estimate = not_a_real_column_name)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = mpg,
        estimate = disp, obviouslywrong = TRUE)
    Condition
      Error in `numeric_metric_summarizer()`:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

# class_metric_summarizer()'s errors when wrong things are passes

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = not_a_real_column_name, estimate = pred)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = obs, estimate = not_a_real_column_name)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = obs, estimate = pred, obviouslywrong = TRUE)
    Condition
      Error in `class_metric_summarizer()`:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

# prob_metric_summarizer()'s errors when wrong things are passes

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, c(HELLO, F, M, L))
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `HELLO` doesn't exist.

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obviouslywrong, VF:L)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `obviouslywrong` doesn't exist.

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, VF:L, obviouslywrong = TRUE)
    Condition
      Error:
      ! Can't subset columns with `TRUE`.
      x `TRUE` must be numeric or character, not `TRUE`.

