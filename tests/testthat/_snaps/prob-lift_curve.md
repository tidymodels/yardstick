# na_rm = FALSE errors if missing values are present

    Code
      lift_curve_vec(df$truth, df$Class1, na_rm = FALSE)
    Condition
      Error in `gain_curve_vec()`:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

# errors with class_pred input

    Code
      lift_curve_vec(cp_truth, estimate)
    Condition
      Error in `gain_curve_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      lift_curve_vec(1, 1, na_rm = "yes")
    Condition
      Error in `lift_curve_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

