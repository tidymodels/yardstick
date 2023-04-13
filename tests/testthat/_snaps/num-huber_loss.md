# Huber Loss

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a positive value.

---

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1, 2))
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a single numeric value.

---

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred", delta = "two")
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a single numeric value.

---

    Code
      huber_loss(ex_dat, truth = "obs", estimate = "pred", delta = -2)
    Condition
      Error in `huber_loss()`:
      ! `delta` must be a positive value.

