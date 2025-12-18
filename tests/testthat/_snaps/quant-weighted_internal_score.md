# Missing value behaviours works

    Code
      weighted_interval_score_vec(truth, preds_na, quantile_levels = 1:9 / 10,
      quantile_estimate_nas = "drop")
    Condition
      Error in `weighted_interval_score_vec()`:
      ! When `quantile_levels` is not a subset of those available in `estimate`, `quantile_estimate_nas` may not be `'drop'`.

