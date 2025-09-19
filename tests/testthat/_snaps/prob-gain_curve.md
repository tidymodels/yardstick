# error handling

    Code
      gain_curve(df, truth, estimate)
    Condition
      Error in `gain_curve()`:
      ! `truth` should be a factor, not a a number.

# na_rm = FALSE errors if missing values are present

    Code
      gain_curve_vec(df$truth, df$Class1, na_rm = FALSE)
    Condition
      Error in `gain_curve_vec()`:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

# errors with class_pred input

    Code
      gain_curve_vec_vec(cp_truth, estimate)
    Condition
      Error in `gain_curve_vec_vec()`:
      ! could not find function "gain_curve_vec_vec"

