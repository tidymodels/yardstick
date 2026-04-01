# `...` is defunct

    Code
      conf_mat(two_class_example, truth, predicted, foo = 1)
    Condition
      Error:
      ! The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0 and is now defunct.
      i This argument no longer has any effect, and is being ignored.

---

    Code
      conf_mat(hpc_cv, obs, pred, foo = 1)
    Condition
      Error:
      ! The `...` argument of `conf_mat()` was deprecated in yardstick 1.0.0 and is now defunct.
      i This argument no longer has any effect, and is being ignored.

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

