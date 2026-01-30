# na_rm argument check

    Code
      gini_coef_vec(1, 1, na_rm = "yes")
    Condition
      Error in `gini_coef_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

# constant truth returns NA with warning

    Code
      result <- gini_coef_vec(truth, estimate)
    Condition
      Warning:
      Column `truth` has constant values.
      i The Gini coefficient is undefined when truth has no variation.

# zero sum truth returns NA with warning

    Code
      result <- gini_coef_vec(truth, estimate)
    Condition
      Warning:
      Column `truth` sums to zero.
      i The Gini coefficient is undefined when the sum of truth is zero.

