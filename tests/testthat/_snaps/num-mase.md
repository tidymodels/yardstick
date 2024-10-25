# Mean Absolute Scaled Error

    Code
      mase_vec(truth, pred, m = "x")
    Condition
      Error in `mase_vec()`:
      ! `m` must be a whole number, not the string "x".

---

    Code
      mase_vec(truth, pred, m = -1)
    Condition
      Error in `mase_vec()`:
      ! `m` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      mase_vec(truth, pred, m = 1.5)
    Condition
      Error in `mase_vec()`:
      ! `m` must be a whole number, not the number 1.5.

---

    Code
      mase_vec(truth, pred, mae_train = -1)
    Condition
      Error in `mase_vec()`:
      ! `mae_train` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      mase_vec(truth, pred, mae_train = "x")
    Condition
      Error in `mase_vec()`:
      ! `mae_train` must be a number or `NULL`, not the string "x".

# mase() errors if m is larger than number of observations

    Code
      mase(mtcars, mpg, disp, m = 100)
    Condition
      Error in `mase()`:
      ! `truth` (32) must have a length greater than `m` (100) to compute the out-of-sample naive mean absolute error.

