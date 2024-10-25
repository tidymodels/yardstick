# `...` is deprecated with a warning

    Code
      conf_mat(two_class_example, truth, predicted, foo = 1)
    Condition
      Warning:
      The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored.
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
      The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0.
      i This argument no longer has any effect, and is being ignored.
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
      Error in `conf_mat()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: virginica, versicolor, and setosa.
      * `estimate`: setosa, versicolor, and virginica.

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = pred, dnn = c("", ""))
    Condition
      Error in `conf_mat()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: 1.
      * `estimate`: setosa, versicolor, and virginica.

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = onelevel, dnn = c("", ""))
    Condition
      Error in `conf_mat()`:
      ! `truth` must have at least 2 factor levels.

# Errors are thrown correctly - grouped

    Code
      conf_mat(three_class, truth = obs_rev, estimate = pred, dnn = c("", ""))
    Condition
      Error in `conf_mat()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: virginica, versicolor, and setosa.
      * `estimate`: setosa, versicolor, and virginica.

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = pred, dnn = c("", ""))
    Condition
      Error in `conf_mat()`:
      x `truth` and `estimate` levels must be equivalent.
      * `truth`: 1.
      * `estimate`: setosa, versicolor, and virginica.

---

    Code
      conf_mat(three_class, truth = onelevel, estimate = onelevel, dnn = c("", ""))
    Condition
      Error in `conf_mat()`:
      ! `truth` must have at least 2 factor levels.

# conf_mat()'s errors when wrong things are passes

    Code
      conf_mat(two_class_example, not_truth, predicted)
    Condition
      Error in `conf_mat()`:
      ! Can't select columns that don't exist.
      x Column `not_truth` doesn't exist.

---

    Code
      conf_mat(two_class_example, truth, not_predicted)
    Condition
      Error in `conf_mat()`:
      ! Can't select columns that don't exist.
      x Column `not_predicted` doesn't exist.

---

    Code
      conf_mat(dplyr::group_by(two_class_example, truth), truth = not_truth,
      estimate = predicted)
    Condition
      Error in `conf_mat()`:
      ! Can't select columns that don't exist.
      x Column `not_truth` doesn't exist.

---

    Code
      conf_mat(dplyr::group_by(two_class_example, truth), truth = truth, estimate = not_predicted)
    Condition
      Error in `conf_mat()`:
      ! Can't select columns that don't exist.
      x Column `not_predicted` doesn't exist.

# conf_mat() error on 1-level factor truth

    Code
      conf_mat(table(1, 1))
    Condition
      Error in `conf_mat()`:
      ! There must be at least 2 factors levels in the `data`.

