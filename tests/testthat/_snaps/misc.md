# validates input types

    Code
      yardstick_table(1, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must be a factor.
      i This is an internal error in the yardstick package, please report it to the package authors.

---

    Code
      yardstick_table(x, 2)
    Condition
      Error in `yardstick_table()`:
      ! `estimate` must be a factor.
      i This is an internal error in the yardstick package, please report it to the package authors.

# levels must be exactly the same

    Code
      yardstick_table(x, y)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error in the yardstick package, please report it to the package authors.

---

    Code
      yardstick_table(x, z)
    Condition
      Error in `yardstick_table()`:
      ! `truth` and `estimate` must have the same levels in the same order.
      i This is an internal error in the yardstick package, please report it to the package authors.

# must have at least 2 levels

    Code
      yardstick_table(x, x)
    Condition
      Error in `yardstick_table()`:
      ! `truth` must have at least 2 factor levels.
      i This is an internal error in the yardstick package, please report it to the package authors.

# case weights must be numeric

    Code
      yardstick_table(x, x, case_weights = "x")
    Condition
      Error in `stop_vctrs()`:
      ! Can't convert <character> to <double>.

