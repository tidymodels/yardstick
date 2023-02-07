# Huber Loss

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `huber_loss_impl()`:
      ! `delta` must be a positive value.

---

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1, 2))
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `huber_loss_impl()`:
      ! `delta` must be a single numeric value.

