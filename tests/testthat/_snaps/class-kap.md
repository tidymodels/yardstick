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

# work with class_pred input

    Code
      kap_vec(cp_truth, cp_estimate)
    Condition
      Error in `kap_vec()`:
      ! `truth` should not a <class_pred> object.

