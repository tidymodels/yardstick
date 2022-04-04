# Huber Loss

    Code
      (expect_error(huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -
        1)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = metric_fn(truth = obs, estimate = pred_na, na_rm = na_rm, delta = -1)`.
      Caused by error in `metric_impl()`:
      ! `delta` must be a positive value.

---

    Code
      (expect_error(huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(
        1, 2))))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `metric_impl()`:
      ! `delta` must be a single numeric value.

