# Huber Loss

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a number larger than or equal to 0, not the number -1.

---

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1, 2))
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a number, not a double vector.

