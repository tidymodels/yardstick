# PR - zero row data frame works

    Code
      out <- pr_curve(df, y, x)
    Condition
      Warning:
      There was 1 warning in `dplyr::reframe()`.
      i In argument: `.estimate = fn(...)`.
      Caused by warning:
      ! There are `0` event cases in `truth`, results will be meaningless.

