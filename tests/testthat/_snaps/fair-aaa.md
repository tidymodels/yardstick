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
      Error:
      ! object 'obs' not found

---

    Code
      bad_by(hpc_cv, truth = obs, estimate = pred)
    Condition
      Error in `dplyr::group_by()`:
      ! Must group by variables found in `.data`.
      x Column `nonexistent_column` is not found.

