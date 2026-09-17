# na_rm argument check

    Code
      huber_loss_vec(1, 1, na_rm = "yes")
    Condition
      Error in `huber_loss_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# bad argument check

    Code
      huber_loss_vec(1, 1, delta = "yes")
    Condition
      Error in `huber_loss_vec()`:
      ! `delta` must be a number, not the string "yes".

