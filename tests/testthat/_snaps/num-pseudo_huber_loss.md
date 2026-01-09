# Pseudo-Huber Loss

    Code
      huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
    Condition
      Error in `huber_loss_pseudo()`:
      ! `delta` must be a number larger than or equal to 0, not the number -1.

---

    Code
      huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1, 2))
    Condition
      Error in `huber_loss_pseudo()`:
      ! `delta` must be a number, not a double vector.

# na_rm argument check

    Code
      huber_loss_pseudo_vec(1, 1, na_rm = "yes")
    Condition
      Error in `huber_loss_pseudo_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

