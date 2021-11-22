# known corner cases are correct

    Code
      out <- average_precision(df, truth, estimate)$.estimate
    Warning <simpleWarning>
      There are `0` event cases in `truth`, results will be meaningless.

---

    Code
      out <- average_precision(df, truth, estimate)$.estimate
    Warning <simpleWarning>
      There are `0` event cases in `truth`, results will be meaningless.

---

    Code
      expect <- pr_auc(df, truth, estimate)$.estimate
    Warning <simpleWarning>
      There are `0` event cases in `truth`, results will be meaningless.

