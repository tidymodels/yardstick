# only 1 unique value of eval_time

    Code
      brier_survival(data = lung_surv, truth = surv_obj, estimate = .pred_survival,
        censoring_weights = prob_censored, eval_time = eval_time)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `fn()`:
      ! `eval_time` should have at most 1 unique value. But 3 was detected.

