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

# curve_metric_summarizer()'s errors when wrong things are passes

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obs, c(HELLO, F, M, L))
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `HELLO` doesn't exist.

---

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obviouslywrong, VF:L)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `obviouslywrong` doesn't exist.

---

    Code
      curve_metric_summarizer(name = "roc_curve", fn = roc_curve_vec, data = hpc_f1,
        truth = obs, VF:L, obviouslywrong = TRUE)
    Condition
      Error:
      ! Can't subset columns with `TRUE`.
      x `TRUE` must be numeric or character, not `TRUE`.

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

# static_survival_metric_summarizer()'s errors with bad input

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = inst, estimate = .pred_time)
    Condition
      Error:
      ! Can't subset columns that don't exist.
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
      ! `estimate` should be a numeric, not a list.

---

    Code
      static_survival_metric_summarizer(name = "concordance_survival", fn = concordance_survival_vec,
        data = lung_surv, truth = surv_obj, estimate = .pred_time, obviouslywrong = TRUE)
    Condition
      Error:
      ! `...` must be empty.
      x Problematic argument:
      * obviouslywrong = TRUE

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

