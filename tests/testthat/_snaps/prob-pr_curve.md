# PR - zero row data frame works

    Code
      out <- pr_curve(df, y, x)
    Condition
      Warning:
      There are 0 event cases in `truth`, results will be meaningless.

# errors with class_pred input

    Code
      pr_curve_vec(cp_truth, estimate)
    Condition
      Error in `pr_curve_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm = FALSE errors if missing values are present

    Code
      pr_curve_vec(df$truth, df$Class1, na_rm = FALSE)
    Condition
      Error in `pr_curve_vec()`:
      x Missing values were detected and `na_ra = FALSE`.
      i Not able to perform calculations.

