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
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimator = finalize_estimator(not_a_real_column_name, metric_class = name)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

---

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = mpg,
        estimate = not_a_real_column_name)
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = fn(truth = mpg, estimate = not_a_real_column_name, na_rm = na_rm)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

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
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimator = finalize_estimator(not_a_real_column_name, estimator, name)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

---

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = obs, estimate = not_a_real_column_name)
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = fn(truth = obs, estimate = not_a_real_column_name, na_rm = na_rm)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

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
        truth = obs, estimate = matrix(data = c(HELLO = HELLO, F = F, M = M, L = L),
        ncol = 4L))
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = fn(...)`.
      Caused by error in `matrix()`:
      ! object 'HELLO' not found

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obviouslywrong, estimate = matrix(data = c(VF = VF, F = F, M = M, L = L),
        ncol = 4L))
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimator = finalize_estimator(obviouslywrong, estimator, name)`.
      Caused by error:
      ! object 'obviouslywrong' not found

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, estimate = matrix(data = c(VF = VF, F = F, M = M, L = L), ncol = 4L),
        obviouslywrong = TRUE)
    Condition
      Error in `prob_metric_summarizer()`:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

