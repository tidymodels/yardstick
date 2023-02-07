# Mean Absolute Scaled Error

    Code
      mase_vec(truth, pred, m = "x")
    Condition
      Error in `validate_m()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, m = -1)
    Condition
      Error in `validate_m()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, m = 1.5)
    Condition
      Error in `validate_m()`:
      ! `m` must be a single positive integer value.

---

    Code
      mase_vec(truth, pred, mae_train = -1)
    Condition
      Error in `validate_mae_train()`:
      ! `mae_train` must be a single positive numeric value.

---

    Code
      mase_vec(truth, pred, mae_train = "x")
    Condition
      Error in `validate_mae_train()`:
      ! `mae_train` must be a single positive numeric value.

