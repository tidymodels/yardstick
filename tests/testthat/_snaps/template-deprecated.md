# metric_summarizer() is soft-deprecated

    Code
      tmp <- metric_summarizer(metric_nm = "rmse", metric_fn = rmse_vec, data = mtcars,
        truth = mpg, estimate = disp, na_rm = TRUE, case_weights = NULL)
    Condition
      Warning:
      `metric_summarizer()` has been soft-deprecated as of version 1.2.0. Please see `?metric_summarizer()` for further instructions.
      This warning is displayed once per session.

