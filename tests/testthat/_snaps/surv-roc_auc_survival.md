# na_rm = FALSE errors if missing values are present

    Code
      roc_curve_survival_vec(truth = lung_surv$surv_obj, lung_surv$.pred, na_rm = FALSE)
    Condition
      Error in `roc_curve_survival_vec()`:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

