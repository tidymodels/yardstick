# errors are thrown if truth or estimate selects more than 1 column

    Code
      rmse(mtcars, mpg, tidyselect::starts_with("d"))
    Condition
      Error in `rmse()`:
      ! `estimate` must select exactly 1 column from `data`, not 2.

---

    Code
      rmse(mtcars, tidyselect::starts_with("d"), mpg)
    Condition
      Error in `rmse()`:
      ! `truth` must select exactly 1 column from `data`, not 2.

# numeric_metric_summarizer()'s errors when wrong things are passes

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = not_a_real_column_name,
        estimate = disp)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = mpg,
        estimate = not_a_real_column_name)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      numeric_metric_summarizer(name = "rmse", fn = rmse_vec, data = mtcars, truth = mpg,
        estimate = disp, obviouslywrong = TRUE)
    Condition
      Error:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

# class_metric_summarizer()'s errors when wrong things are passes

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = not_a_real_column_name, estimate = pred)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = obs, estimate = not_a_real_column_name)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `not_a_real_column_name` doesn't exist.

---

    Code
      class_metric_summarizer(name = "accuracy", fn = accuracy_vec, data = three_class,
        truth = obs, estimate = pred, obviouslywrong = TRUE)
    Condition
      Error:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

# prob_metric_summarizer()'s errors when wrong things are passes

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, c(HELLO, F, M, L))
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `HELLO` doesn't exist.

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obviouslywrong, VF:L)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `obviouslywrong` doesn't exist.

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, VF:L, obviouslywrong = TRUE)
    Condition
      Error:
      ! Can't select columns with `TRUE`.
      x `TRUE` must be numeric or character, not `TRUE`.

---

    Code
      prob_metric_summarizer(name = "roc_auc", fn = roc_auc_vec, data = hpc_f1,
        truth = obs, estimate = VF:L)
    Condition
      Error:
      x This metric doesn't use the `estimate` argument.
      i Specify the columns without `estimate = `.

# curve_metric_summarizer()'s na_rm argument work

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1_na,
        truth = obs, VF:L, na_rm = FALSE, case_weights = NULL)
    Condition
      Error:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

# curve_metric_summarizer()'s errors when wrong things are passes

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obs, c(HELLO, F, M, L))
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `HELLO` doesn't exist.

---

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obviouslywrong, VF:L)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `obviouslywrong` doesn't exist.

---

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obs, VF:L, obviouslywrong = TRUE)
    Condition
      Error:
      ! Can't select columns with `TRUE`.
      x `TRUE` must be numeric or character, not `TRUE`.

---

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obs, estimate = VF:L)
    Condition
      Error:
      x This metric doesn't use the `estimate` argument.
      i Specify the columns without `estimate = `.

# dynamic_survival_metric_summarizer()'s errors with bad input

    Code
      dynamic_survival_metric_summarizer(name = "brier_survival", fn = brier_survival_vec,
        data = lung_surv, truth = .pred_time, .pred)
    Condition
      Error:
      ! `truth` should be a Surv object, not a a double vector.

---

    Code
      dynamic_survival_metric_summarizer(name = "brier_survival", fn = brier_survival_vec,
        data = lung_surv, truth = surv_obj, surv_obj)
    Condition
      Error:
      ! `estimate` should be a list, not a a <Surv> object.

---

    Code
      dynamic_survival_metric_summarizer(name = "brier_survival", fn = brier_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = .pred)
    Condition
      Error:
      x This metric doesn't use the `estimate` argument.
      i Specify the columns without `estimate = `.

# static_survival_metric_summarizer()'s errors with bad input

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = inst, estimate = .pred_time)
    Condition
      Error:
      ! Can't select columns that don't exist.
      x Column `inst` doesn't exist.

---

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = surv_obj)
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a numeric matrix.

---

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = list)
    Condition
      Error:
      ! `estimate` should be a numeric vector, not a list.

---

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = .pred_time, obviouslywrong = TRUE)
    Condition
      Error:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

# curve_survival_metric_summarizer()'s na_rm argument works

    Code
      curve_survival_metric_summarizer(name = "roc_curve_survival", fn = roc_curve_survival_vec,
        data = lung_surv, truth = surv_obj, .pred, na_rm = FALSE, case_weights = NULL)
    Condition
      Error:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

# curve_survival_metric_summarizer()'s errors with bad input

    Code
      curve_survival_metric_summarizer(name = "roc_curve_survival", fn = roc_curve_survival_vec,
        data = lung_surv, truth = .pred_time, .pred)
    Condition
      Error:
      ! `truth` should be a Surv object, not a a double vector.

---

    Code
      curve_survival_metric_summarizer(name = "roc_curve_survival", fn = roc_curve_survival_vec,
        data = lung_surv, truth = surv_obj, surv_obj)
    Condition
      Error:
      ! `estimate` should be a list, not a a <Surv> object.

---

    Code
      curve_survival_metric_summarizer(name = "roc_curve_survival", fn = roc_curve_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = .pred)
    Condition
      Error:
      x This metric doesn't use the `estimate` argument.
      i Specify the columns without `estimate = `.

