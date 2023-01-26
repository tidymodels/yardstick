# only 1 unique value of .time

    Code
      brier_survival(data = lung_surv, truth = surv_obj, estimate = .pred_survival,
        censoring_weights = prob_censored, .time = .time)
    Condition
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = fn(...)`.
      Caused by error in `fn()`:
      ! `.time` should have at most 1 unique value. But 3 was detected.

