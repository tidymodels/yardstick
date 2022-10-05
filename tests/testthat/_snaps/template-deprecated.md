# metric_summarizer() is soft-deprecated

    Code
      tmp <- metric_summarizer(metric_nm = "rmse", metric_fn = rmse_vec, data = mtcars,
        truth = mpg, estimate = disp, na_rm = TRUE, case_weights = NULL)
    Condition
      Warning:
      `metric_summarizer()` was deprecated in yardstick 1.2.0.
      Please use `numeric_metric_summarizer()`, `class_metric_summarizer()`, or `prob_metric_summarizer()` instead.

# metric_summarizer()'s errors when wrong things are passes

    Code
      metric_summarizer(metric_nm = "rmse", metric_fn = rmse_vec, data = mtcars,
        truth = not_a_real_column_name, estimate = disp)
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimator = eval_tidy(finalize_estimator_expr)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

---

    Code
      metric_summarizer(metric_nm = "rmse", metric_fn = rmse_vec, data = mtcars,
        truth = mpg, estimate = not_a_real_column_name)
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = metric_fn(truth = mpg, estimate = not_a_real_column_name, na_rm = na_rm)`.
      Caused by error:
      ! object 'not_a_real_column_name' not found

