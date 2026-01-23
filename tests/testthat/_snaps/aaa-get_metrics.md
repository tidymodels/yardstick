# get_metrics() returns metric_set

    Code
      res
    Output
      A metric set, consisting of:
      - `rmse()`, a numeric metric              | direction: minimize
      - `mpe()`, a numeric metric               | direction: zero
      - `msd()`, a numeric metric               | direction: zero
      - `smape()`, a numeric metric             | direction: minimize
      - `rpiq()`, a numeric metric              | direction: maximize
      - `ccc()`, a numeric metric               | direction: maximize
      - `rsq_trad()`, a numeric metric          | direction: maximize
      - `poisson_log_loss()`, a numeric metric  | direction: minimize
      - `huber_loss()`, a numeric metric        | direction: minimize
      - `mape()`, a numeric metric              | direction: minimize
      - `mae()`, a numeric metric               | direction: minimize
      - `mase()`, a numeric metric              | direction: minimize
      - `rpd()`, a numeric metric               | direction: maximize
      - `iic()`, a numeric metric               | direction: maximize
      - `rsq()`, a numeric metric               | direction: maximize
      - `huber_loss_pseudo()`, a numeric metric | direction: minimize

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
      ! `type` must be one of "class", "integrated_survival", "linear_pred_survival", "ordered_prob", "groupwise", "dynamic_survival", "static_survival", "prob", "quantile", or "numeric", not "invalid".

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
      - class (j_index, sensitivity, fall_out, f_meas, sens, bal_accuracy, detection_prevalence, recall, spec, accuracy, kap, npv, mcc, precision, miss_rate, specificity, ppv)
      - numeric (rmse, mpe, msd, smape, rpiq, ccc, rsq_trad, poisson_log_loss, huber_loss, mape, mae, mase, rpd, iic, rsq, huber_loss_pseudo)

