# AUNP errors on binary case

    Code
      (expect_error(roc_aunp(two_class_example, truth, Class1)))
    Output
      <error/rlang_error>
      Error in `dplyr::summarise()`:
      ! Problem while computing `.estimate = metric_fn(truth = truth, estimate = Class1, na_rm = na_rm, options = list())`.
      Caused by error in `multiclass_checks.matrix()`:
      ! The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

