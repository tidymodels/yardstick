# `...` is deprecated with a warning

    Code
      conf_mat(two_class_example, truth, predicted, foo = 1)
    Condition
      Warning:
      The `...` argument of `conf_mat()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
    Output
                Truth
      Prediction Class1 Class2
          Class1    227     50
          Class2     31    192

---

    Code
      conf_mat(hpc_cv, obs, pred, foo = 1)
    Condition
      Warning:
      The `...` argument of `conf_mat()` is deprecated as of yardstick 1.0.0.
      This argument no longer has any effect, and is being ignored.
    Output
      # A tibble: 10 x 2
         Resample conf_mat  
         <chr>    <list>    
       1 Fold01   <conf_mat>
       2 Fold02   <conf_mat>
       3 Fold03   <conf_mat>
       4 Fold04   <conf_mat>
       5 Fold05   <conf_mat>
       6 Fold06   <conf_mat>
       7 Fold07   <conf_mat>
       8 Fold08   <conf_mat>
       9 Fold09   <conf_mat>
      10 Fold10   <conf_mat>

# Errors are thrown correctly

    Code
      conf_mat(three_class, truth = obs_rev, estimate = pred, dnn = c("", ""))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: virginica, versicolor, setosa
      `estimate`: setosa, versicolor, virginica

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = pred, dnn = c("", ""))
    Condition
      Error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: 1
      `estimate`: setosa, versicolor, virginica

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = onelevel, dnn = c("", ""))
    Condition
      Error in `conf_mat_impl()`:
      ! `truth` must have at least 2 factor levels.

# Errors are thrown correctly - grouped

    Code
      conf_mat(three_class, truth = obs_rev, estimate = pred, dnn = c("", ""))
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `conf_mat = { ... }`.
      i In group 1: `pred = setosa`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: virginica, versicolor, setosa
      `estimate`: setosa, versicolor, virginica

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = pred, dnn = c("", ""))
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `conf_mat = { ... }`.
      i In group 1: `pred = setosa`.
      Caused by error in `validate_factor_truth_factor_estimate()`:
      ! `truth` and `estimate` levels must be equivalent.
      `truth`: 1
      `estimate`: setosa, versicolor, virginica

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = onelevel, dnn = c("", ""))
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `conf_mat = { ... }`.
      i In group 1: `pred = setosa`.
      Caused by error in `conf_mat_impl()`:
      ! `truth` must have at least 2 factor levels.

