# metrics() - `options` is deprecated

    Code
      out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `metrics()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

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

# propagates 'caused by' error message when specifying the wrong column name

    Code
      set(two_class_example, truth, Class1, estimate = predicted, case_weights = weight)
    Condition
      Error:
      ! In metric: `accuracy`
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error:
      ! object 'weight' not found

