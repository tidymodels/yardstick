# Mean Absolute Scaled Error

    Code
      mase_vec(truth, pred, m = "x")
    Condition
      Error in `mase_impl()`:
      ! `m` must be a whole number, not the string "x".

---

    Code
      mase_vec(truth, pred, m = -1)
    Condition
      Error in `mase_impl()`:
      ! `m` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      mase_vec(truth, pred, m = 1.5)
    Condition
      Error in `mase_impl()`:
      ! `m` must be a whole number, not the number 1.5.

---

    Code
      mase_vec(truth, pred, mae_train = -1)
    Condition
      Error in `mase_impl()`:
      ! `mae_train` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      mase_vec(truth, pred, mae_train = "x")
    Condition
      Error in `mase_impl()`:
      ! `mae_train` must be a number or `NULL`, not the string "x".

