# `fn` is validated

    Code
      new_class_metric(1, "maximize")
    Condition
      Error in `new_class_metric()`:
      ! `fn` must be a function, not the number 1.

# `direction` is validated

    Code
      new_class_metric(function() 1, "min")
    Condition
      Error in `new_class_metric()`:
      ! `direction` must be one of "maximize", "minimize", or "zero", not "min".
      i Did you mean "minimize"?

# metric print method works

    Code
      rmse
    Output
      A numeric metric | direction: minimize

---

    Code
      roc_auc
    Output
      A probability metric | direction: maximize

---

    Code
      demographic_parity(boop)
    Output
      A class metric | direction: minimize, group-wise on: boop

