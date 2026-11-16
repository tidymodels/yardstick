# na_rm argument check

    Code
      mase_vec(1, 1, na_rm = "yes")
    Condition
      Error in `mase_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# bad argument check

    Code
      mase_vec(1, 1, m = "yes")
    Condition
      Error in `mase_vec()`:
      ! `m` must be a whole number, not the string "yes".

# m argument check - errors if m is less than 1

    Code
      mase_vec(1, 1, m = 0)
    Condition
      Error in `mase_vec()`:
      ! `m` must be a whole number larger than or equal to 1, not the number 0.

# mase() - errors if m is larger than number of observations

    Code
      mase(mtcars, mpg, disp, m = 100)
    Condition
      Error in `mase()`:
      ! `truth` (32) must have a length greater than `m` (100) to compute the out-of-sample naive mean absolute error.

