# yardstick correlation warnings are thrown

    Code
      (expect_warning(object = out <- rsq_vec(1, 1), class = "yardstick_warning_correlation_undefined_size_zero_or_one")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_size_zero_or_one>
      Warning:
      A correlation computation is required, but the inputs are size zero or one and the standard deviation cannot be computed. `NA` will be returned.

---

    Code
      (expect_warning(object = out <- rsq_vec(double(), double()), class = "yardstick_warning_correlation_undefined_size_zero_or_one")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_size_zero_or_one>
      Warning:
      A correlation computation is required, but the inputs are size zero or one and the standard deviation cannot be computed. `NA` will be returned.

---

    Code
      (expect_warning(object = out <- rsq_vec(c(1, 2), c(1, 1)), class = "yardstick_warning_correlation_undefined_constant_estimate")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_constant_estimate>
      Warning:
      A correlation computation is required, but `estimate` is constant and has 0 standard deviation, resulting in a divide by 0 error. `NA` will be returned.

---

    Code
      (expect_warning(object = out <- rsq_vec(c(1, 1), c(1, 2)), class = "yardstick_warning_correlation_undefined_constant_truth")
      )
    Output
      <warning/yardstick_warning_correlation_undefined_constant_truth>
      Warning:
      A correlation computation is required, but `truth` is constant and has 0 standard deviation, resulting in a divide by 0 error. `NA` will be returned.

