# validates input types

    Code
      yardstick_table(1, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must be a factor.
      i This is an internal error in the yardstick package, please report it to the package authors.

---

    Code
      yardstick_table(x, 2)
    Condition
      Error in `yardstick_table()`:
      ! `estimate` must be a factor.
      i This is an internal error in the yardstick package, please report it to the package authors.

# levels must be exactly the same

    Code
      yardstick_table(x, y)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error in the yardstick package, please report it to the package authors.

---

    Code
      yardstick_table(x, z)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error in the yardstick package, please report it to the package authors.

# must have at least 2 levels

    Code
      yardstick_table(x, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must have at least 2 factor levels.
      i This is an internal error in the yardstick package, please report it to the package authors.

# case weights must be numeric

    Code
      yardstick_table(x, x, case_weights = "x")
    Condition
      Error in `hardhat::weighted_table()`:
      ! Can't convert `weights` <character> to <double>.

# works with constant inputs

    Code
      (expect_warning(object = out <- yardstick_cor(c(1, 2), c(1, 1)), class = "yardstick_warning_correlation_undefined_constant_estimate")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_constant_estimate>
      Warning:
      A correlation computation is required, but `estimate` is constant and has 0 standard deviation, resulting in a divide by 0 error. `NA` will be returned.

---

    Code
      (expect_warning(object = out <- yardstick_cor(c(1, 1), c(1, 2)), class = "yardstick_warning_correlation_undefined_constant_truth")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_constant_truth>
      Warning:
      A correlation computation is required, but `truth` is constant and has 0 standard deviation, resulting in a divide by 0 error. `NA` will be returned.

# warns with input of size 1

    Code
      (expect_warning(object = out <- yardstick_cor(1, 1), class = "yardstick_warning_correlation_undefined_size_zero_or_one")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_size_zero_or_one>
      Warning:
      A correlation computation is required, but the inputs are size zero or one and the standard deviation cannot be computed. `NA` will be returned.

# warns with input of size 0

    Code
      (expect_warning(object = out <- yardstick_cor(double(), double()), class = "yardstick_warning_correlation_undefined_size_zero_or_one")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_size_zero_or_one>
      Warning:
      A correlation computation is required, but the inputs are size zero or one and the standard deviation cannot be computed. `NA` will be returned.

