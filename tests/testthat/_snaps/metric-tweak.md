# cannot use protected names

    Code
      metric_tweak("f_meas2", f_meas, data = 2)
    Condition
      Error in `check_protected_names()`:
      ! Arguments passed through `...` cannot be named any of: 'data', 'truth', 'estimate'.

---

    Code
      metric_tweak("f_meas2", f_meas, truth = 2)
    Condition
      Error in `check_protected_names()`:
      ! Arguments passed through `...` cannot be named any of: 'data', 'truth', 'estimate'.

---

    Code
      metric_tweak("f_meas2", f_meas, estimate = 2)
    Condition
      Error in `check_protected_names()`:
      ! Arguments passed through `...` cannot be named any of: 'data', 'truth', 'estimate'.

# `name` must be a string

    Code
      metric_tweak(1, f_meas, beta = 2)
    Condition
      Error in `metric_tweak()`:
      ! `.name` must be a string.

# `fn` must be a metric function

    Code
      metric_tweak("foo", function() { }, beta = 2)
    Condition
      Error in `metric_tweak()`:
      ! `.fn` must be a metric function.

# All `...` must be named

    Code
      metric_tweak("foo", accuracy, 1)
    Condition
      Error in `metric_tweak()`:
      ! All arguments passed through `...` must be named.

