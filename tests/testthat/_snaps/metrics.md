# bad args

    Code
      metrics(two_class_example, truth, Class1)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `accuracy()`.
      Caused by error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `estimate` should be a factor, not a `numeric`.

---

    Code
      metrics(two_class_example, Class1, truth)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `rmse()`.
      Caused by error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_numeric_truth_numeric_estimate()`:
      ! `estimate` should be a numeric, not a `factor`.

---

    Code
      metrics(three_class, "obs", "pred", setosa, versicolor)
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `.estimate = fn(...)`.
      Caused by error in `validate_factor_truth_matrix_estimate()`:
      ! The number of levels in `truth` (3) must match the number of columns supplied in `...` (2).

# metrics() - `options` is deprecated

    Code
      out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `metrics()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored. Use the pROC package directly if you need these features.

# numeric metric sets

    Code
      metric_set(rmse, "x")
    Condition
      Error:
      ! All inputs to `metric_set()` must be functions. These inputs are not: (2).

# mixing bad metric sets

    Code
      metric_set(rmse, accuracy)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only numeric metrics
      - a mix of class metrics and class probability metrics
      
      The following metric function types are being mixed:
      - numeric (rmse)
      - class (accuracy)

# print metric_set works

    Code
      metric_set(rmse, rsq, ccc)
    Output
      # A tibble: 3 x 3
        metric class          direction
        <chr>  <chr>          <chr>    
      1 rmse   numeric_metric minimize 
      2 rsq    numeric_metric maximize 
      3 ccc    numeric_metric maximize 

# `metric_set()` errors contain env name for unknown functions (#128)

    Code
      metric_set(accuracy, foobar, sens, rlang::abort)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only numeric metrics
      - a mix of class metrics and class probability metrics
      
      The following metric function types are being mixed:
      - class (accuracy, sens)
      - other (foobar <test>, abort <namespace:rlang>)

---

    Code
      metric_set(accuracy, foobar, sens, rlang::abort)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only numeric metrics
      - a mix of class metrics and class probability metrics
      
      The following metric function types are being mixed:
      - class (accuracy, sens)
      - other (foobar <test>, abort <namespace:rlang>)

# `metric_set()` gives an informative error for a single non-metric function (#181)

    Code
      metric_set(foobar)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only numeric metrics
      - a mix of class metrics and class probability metrics
      
      The following metric function types are being mixed:
      - other (foobar <test>)

# propagates 'caused by' error message when specifying the wrong column name

    Code
      set(two_class_example, truth, Class1, estimate = predicted, case_weights = weight)
    Condition
      Error in `metric_set()`:
      ! Failed to compute `accuracy()`.
      Caused by error:
      ! Can't subset columns that don't exist.
      x Column `weight` doesn't exist.

