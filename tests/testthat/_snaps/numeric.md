# Huber Loss

    Code
      (expect_error(huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -
        1)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = obs, estimate = pred_na, na_rm = na_rm, delta = -1)`.
      x `delta` must be a positive value.

---

    Code
      (expect_error(huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(
        1, 2))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `delta` must be a single numeric value.

# Pseudo-Huber Loss

    Code
      (expect_error(huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na",
        delta = -1)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = obs, estimate = pred_na, na_rm = na_rm, delta = -1)`.
      x `delta` must be a positive value.

---

    Code
      (expect_error(huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na",
        delta = c(1, 2))))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(...)`.
      x `delta` must be a single numeric value.

