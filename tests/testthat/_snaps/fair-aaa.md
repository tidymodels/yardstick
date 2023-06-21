# handles `direction` input

    Code
      fairness_metric(sens, "bad_direction", diff_range, direction = "boop")
    Condition
      Error in `fairness_metric()`:
      ! `direction` must be one of "maximize", "minimize", or "zero", not "boop".

# errors informatively with bad input

    Code
      fairness_metric()
    Condition
      Error in `fairness_metric()`:
      ! `.fn` must be a metric function or metric set.

---

    Code
      fairness_metric(sens)
    Condition
      Error in `fairness_metric()`:
      ! `.name` must be a string.

---

    Code
      fairness_metric(sens, "bop")
    Condition
      Error in `fairness_metric()`:
      ! `.post` must be a function.

---

    Code
      fairness_metric("boop", "bop", identity)
    Condition
      Error in `fairness_metric()`:
      ! `.fn` must be a metric function or metric set.

---

    Code
      fairness_metric(identity, "bop", identity)
    Condition
      Error in `fairness_metric()`:
      ! `.fn` must be a metric function or metric set.

---

    Code
      fairness_metric(sens, 1, identity)
    Condition
      Error in `fairness_metric()`:
      ! `.name` must be a string.

---

    Code
      fairness_metric(sens, "bop", "boop")
    Condition
      Error in `fairness_metric()`:
      ! `.post` must be a function.

# outputted function errors informatively with bad input

    Code
      bad_.post(Resample)(hpc_cv, truth = obs, estimate = pred)
    Condition
      Error in `fairness_metric()`:
      ! `.post` must return a single numeric value.

---

    Code
      bad_by(hpc_cv, truth = obs, estimate = pred)
    Condition
      Error in `dplyr::group_by()`:
      ! Must group by variables found in `.data`.
      x Column `nonexistent_column` is not found.

---

    Code
      bad_truth_metric_set(hpc_cv, truth = VF, estimate = pred)
    Condition
      Error in `bop()`:
      ! `truth` should be a factor, not a `numeric`.

---

    Code
      bad_truth_metric(hpc_cv, truth = VF, estimate = pred)
    Condition
      Error in `bop()`:
      ! `truth` should be a factor, not a `numeric`.

# outputted function errors informatively with redundant grouping

    Code
      hpc_cv %>% dplyr::group_by(Resample) %>% demographic_parity(Resample)(truth = obs,
        estimate = pred)
    Condition
      Error:
      ! Metric is internally grouped by Resample; grouping `data` by Resample is not well-defined.

---

    Code
      hpc_cv %>% dplyr::group_by(Resample) %>% dp_res(truth = obs, estimate = pred)
    Condition
      Error in `dp_res()`:
      ! Metric is internally grouped by Resample; grouping `data` by Resample is not well-defined.

