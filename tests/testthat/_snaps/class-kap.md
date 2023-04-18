# kap errors with wrong `weighting`

    Code
      kap(three_class, truth = "obs", estimate = "pred", weighting = 1)
    Condition
      Error in `kap()`:
      ! `weighting` must be a string.

---

    Code
      kap(three_class, truth = "obs", estimate = "pred", weighting = "not right")
    Condition
      Error in `kap()`:
      ! `weighting` must be 'none', 'linear', or 'quadratic'.

