# AUNP errors on binary case

    Code
      (expect_error(roc_aunp(two_class_example, truth, Class1)))
    Output
      <error/dplyr_error>
      Problem with `summarise()` column `.estimate`.
      i `.estimate = metric_fn(truth = truth, estimate = Class1, na_rm = na_rm, options = list())`.
      x The number of levels in `truth` (2) must match the number of columns supplied in `...` (1).

