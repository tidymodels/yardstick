# metric_summarizer() is soft-deprecated

    Code
      tmp <- metric_summarizer(metric_nm = "rmse", metric_fn = rmse_vec, data = mtcars,
        truth = mpg, estimate = disp, na_rm = TRUE, case_weights = NULL)
    Condition
      Warning:
      `metric_summarizer()` was deprecated in yardstick 1.2.0.
      Please use `numeric_metric_summarizer()`, `class_metric_summarizer()`, or `prob_metric_summarizer()` instead.

