# PR - zero row data frame works

    Code
      out <- pr_curve(df, y, x)
    Condition
      Warning:
      There are `0` event cases in `truth`, results will be meaningless.

# errors with class_pred input

    Code
      pr_curve_vec(cp_truth, estimate)
    Condition
      Error in `pr_curve_vec()`:
      ! `truth` should not a <class_pred> object.

