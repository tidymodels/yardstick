# known corner cases are correct

    Code
      out <- average_precision(df, truth, estimate)$.estimate
    Condition
      Warning:
      There are 0 event cases in `truth`, results will be meaningless.

---

    Code
      out <- average_precision(df, truth, estimate)$.estimate
    Condition
      Warning:
      There are 0 event cases in `truth`, results will be meaningless.

---

    Code
      expect <- pr_auc(df, truth, estimate)$.estimate
    Condition
      Warning:
      There are 0 event cases in `truth`, results will be meaningless.

# errors with class_pred input

    Code
      average_precision_vec(cp_truth, estimate)
    Condition
      Error in `average_precision_vec()`:
      ! `truth` should not a <class_pred> object.

# na_rm argument check

    Code
      average_precision_vec(1, 1, na_rm = "yes")
    Condition
      Error in `average_precision_vec()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "yes".

