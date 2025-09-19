# metric factory print method works

    Code
      equal_opportunity
    Output
      A metric factory (`?yardstick::new_groupwise_metric()`)

# handles `direction` input

    Code
      new_groupwise_metric(sens, "bad_direction", diff_range, direction = "boop")
    Condition
      Error in `new_groupwise_metric()`:
      ! `direction` must be one of "maximize", "minimize", or "zero", not "boop".

# errors informatively with bad input

    Code
      new_groupwise_metric()
    Condition
      Error in `new_groupwise_metric()`:
      ! `fn` must be a metric function or metric set.

---

    Code
      new_groupwise_metric(sens)
    Condition
      Error in `new_groupwise_metric()`:
      ! `name` must be a string.

---

    Code
      new_groupwise_metric(sens, "bop")
    Condition
      Error in `new_groupwise_metric()`:
      ! `aggregate` must be a function.

---

    Code
      new_groupwise_metric("boop", "bop", identity)
    Condition
      Error in `new_groupwise_metric()`:
      ! `fn` must be a metric function or metric set.

---

    Code
      new_groupwise_metric(identity, "bop", identity)
    Condition
      Error in `new_groupwise_metric()`:
      ! `fn` must be a metric function or metric set.

---

    Code
      new_groupwise_metric(sens, 1, identity)
    Condition
      Error in `new_groupwise_metric()`:
      ! `name` must be a string.

---

    Code
      new_groupwise_metric(sens, "bop", "boop")
    Condition
      Error in `new_groupwise_metric()`:
      ! `aggregate` must be a function.

# outputted function errors informatively with bad input

    Code
      bad_aggregate(Resample)(hpc_cv, truth = obs, estimate = pred)
    Condition
      Error in `new_groupwise_metric()`:
      ! `aggregate` must return a single numeric value.

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
      ! `truth` should be a factor, not a a double vector.

---

    Code
      bad_truth_metric(hpc_cv, truth = VF, estimate = pred)
    Condition
      Error in `bop()`:
      ! `truth` should be a factor, not a a double vector.

# outputted function errors informatively with redundant grouping

    Code
      demographic_parity(Resample)(dplyr::group_by(hpc_cv, Resample), truth = obs,
      estimate = pred)
    Condition
      Error:
      ! Metric is internally grouped by Resample; grouping `data` by Resample is not well-defined.

---

    Code
      dp_res(dplyr::group_by(hpc_cv, Resample), truth = obs, estimate = pred)
    Condition
      Error in `dp_res()`:
      ! Metric is internally grouped by Resample; grouping `data` by Resample is not well-defined.

