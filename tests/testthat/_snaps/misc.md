# validates input types

    Code
      yardstick_table(1, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must be a factor, not a number.
      i This is an internal error that was detected in the yardstick package.
        Please report it at <https://github.com/tidymodels/yardstick/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      yardstick_table(x, 2)
    Condition
      Error in `yardstick_table()`:
      ! `estimate` must be a factor, not a number.
      i This is an internal error that was detected in the yardstick package.
        Please report it at <https://github.com/tidymodels/yardstick/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# levels must be exactly the same

    Code
      yardstick_table(x, y)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error that was detected in the yardstick package.
        Please report it at <https://github.com/tidymodels/yardstick/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      yardstick_table(x, z)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error that was detected in the yardstick package.
        Please report it at <https://github.com/tidymodels/yardstick/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# must have at least 2 levels

    Code
      yardstick_table(x, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must have at least 2 factor levels.
      i This is an internal error that was detected in the yardstick package.
        Please report it at <https://github.com/tidymodels/yardstick/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

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

# `x` is validated

    Code
      weighted_quantile("x", 1, 0.5)
    Condition
      Error in `weighted_quantile()`:
      ! Can't convert `x` <character> to <double>.

# `weights` is validated

    Code
      weighted_quantile(1, "x", 0.5)
    Condition
      Error in `weighted_quantile()`:
      ! Can't convert `weights` <character> to <double>.

# `x` and `weights` must be the same size

    Code
      weighted_quantile(1, 1:2, 0.5)
    Condition
      Error in `weighted_quantile()`:
      ! `x` (1) and `weights` (2) must have the same size.

# `probabilities` is validated

    Code
      weighted_quantile(1, 1, "x")
    Condition
      Error in `weighted_quantile()`:
      ! Can't convert `probabilities` <character> to <double>.

# `probabilities` must be in [0, 1]

    Code
      weighted_quantile(1, 1, -1)
    Condition
      Error in `weighted_quantile()`:
      ! `probabilities` must be within `[0, 1]`.

---

    Code
      weighted_quantile(1, 1, 2)
    Condition
      Error in `weighted_quantile()`:
      ! `probabilities` must be within `[0, 1]`.

# `probabilities` can't be missing

    Code
      weighted_quantile(1, 1, NA)
    Condition
      Error in `weighted_quantile()`:
      ! `probabilities` can't have missing values.

# work with class_pred input

    Code
      accuracy_vec(fct_truth, cp_estimate)
    Condition
      Error in `as_factor_from_class_pred()`:
      ! A <class_pred> input was detected, but the probably package isn't installed. Install probably to be able to convert <class_pred> to <factor>.

