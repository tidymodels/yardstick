# roc_curve() - `options` is deprecated

    Code
      out <- roc_curve(two_class_example, truth, Class1, options = 1)
    Condition
      Warning:
      The `options` argument of `roc_curve()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
      Use the pROC package directly if you need these features.

