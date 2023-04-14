# Mean Absolute Scaled Error

    Code
      mase_vec(truth, pred, m = "x")
    Condition
      Error in `mase_vec()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, m = -1)
    Condition
      Error in `mase_vec()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, m = 1.5)
    Condition
      Error in `mase_vec()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, mae_train = -1)
    Condition
      Error in `mase_vec()`:
      ! `mae_train` must be a single positive numeric value.

---

    Code
      mase_vec(truth, pred, mae_train = "x")
    Condition
      Error in `mase_vec()`:
      ! `mae_train` must be a single positive numeric value.

