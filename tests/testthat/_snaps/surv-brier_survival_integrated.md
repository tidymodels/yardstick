# brier_survival_integrated calculations

    Code
      brier_survival_integrated(data = lung_surv, truth = surv_obj, .pred)
    Condition
      Error in `brier_survival_integrated()`:
      ! At least 2 evaluation times are required. Only 1 unique time was given.

# na_rm argument check

    Code
      brier_survival_integrated_vec(1, 1, na_rm = "yes")
    Condition
      Error in `brier_survival_integrated_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

