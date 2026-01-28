# get_metrics() returns metric_set

    Code
      res
    Output
      A metric set, consisting of:
      - `ccc()`, a numeric metric               | direction: maximize
      - `huber_loss()`, a numeric metric        | direction: minimize
      - `huber_loss_pseudo()`, a numeric metric | direction: minimize
      - `iic()`, a numeric metric               | direction: maximize
      - `mae()`, a numeric metric               | direction: minimize
      - `mape()`, a numeric metric              | direction: minimize
      - `mase()`, a numeric metric              | direction: minimize
      - `mpe()`, a numeric metric               | direction: zero
      - `msd()`, a numeric metric               | direction: zero
      - `poisson_log_loss()`, a numeric metric  | direction: minimize
      - `rmse()`, a numeric metric              | direction: minimize
      - `rmse_relative()`, a numeric metric     | direction: minimize
      - `rpd()`, a numeric metric               | direction: maximize
      - `rpiq()`, a numeric metric              | direction: maximize
      - `rsq()`, a numeric metric               | direction: maximize
      - `rsq_trad()`, a numeric metric          | direction: maximize
      - `smape()`, a numeric metric             | direction: minimize

# get_metrics() errors on empty input

    Code
      get_metrics()
    Condition
      Error in `get_metrics()`:
      ! `type` must be a character vector, not absent.

# get_metrics() errors on invalid type

    Code
      get_metrics("invalid")
    Condition
      Error in `get_metrics()`:
      ! `type` must be one of "class", "dynamic_survival", "groupwise", "integrated_survival", "linear_pred_survival", "numeric", "ordered_prob", "prob", "quantile", or "static_survival", not "invalid".

# get_metrics() errors on incompatible metric types

    Code
      get_metrics(c("class", "numeric"))
    Condition
      Error in `metric_set()`:
      x The combination of metric functions must be:
      * only numeric metrics.
      * a mix of class metrics and class probability metrics.
      * a mix of dynamic and static survival metrics.
      i The following metric function types are being mixed:
      - class (accuracy, bal_accuracy, detection_prevalence, f_meas, fall_out, j_index, kap, mcc, miss_rate, npv, ppv, precision, recall, sens, sensitivity, spec, specificity)
      - numeric (ccc, huber_loss, huber_loss_pseudo, iic, mae, mape, mase, mpe, msd, poisson_log_loss, rmse, rmse_relative, rpd, rpiq, rsq, rsq_trad, smape)

