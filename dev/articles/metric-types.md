# Metric types

## Metric types

There are different main metric types in `yardstick`: class, class
probability, numeric, and survival. Each type of metric has standardized
argument syntax, and all metrics return the same kind of output (a
tibble with 3 columns). This standardization allows metrics to easily be
grouped together and used with grouped data frames for computing on
multiple resamples at once. Below are the different types of metrics,
along with the types of the inputs they take.

1.  **Numeric metrics**

    - `truth` - numeric

    - `estimate` - numeric

2.  **Class metrics** (hard predictions)

    - `truth` - factor

    - `estimate` - factor

3.  **Class probability metrics** (soft predictions)

    - `truth` - factor

    - `estimate / ...` - multiple numeric columns containing class
      probabilities

4.  **Ordered probability metrics** (soft predictions)

    - `truth` - ordered factor

    - `estimate / ...` - multiple numeric columns containing class
      probabilities

5.  **Static survival metrics**

    - `truth` - Surv

    - `estimate` - numeric

6.  **Dynamic survival metrics** (one value per evaluation time)

    - `truth` - Surv

    - `...` - list of data.frames, each containing the 3 columns
      `.eval_time`, `.pred_survival`, and `.weight_censored`

7.  **Integrated survival metrics** (one value across evaluation times)

    - `truth` - Surv

    - `...` - list of data.frames, each containing the 3 columns
      `.eval_time`, `.pred_survival`, and `.weight_censored`

8.  **Linear predictor survival metrics**

    - `truth` - Surv

    - `estimate` - numeric

9.  **Quantile metrics**

    - `truth` - numeric

    - `estimate` - quantile_pred

## Example

In the following example, the `hpc_cv` data set is used. It contains
class probabilities and class predictions for a linear discriminant
analysis fit to the HPC data set of Kuhn and Johnson (2013). It is fit
with 10 fold cross-validation, and the predictions for all folds are
included.

``` r
library(yardstick)
library(dplyr)
data("hpc_cv")

hpc_cv |>
  group_by(Resample) |>
  slice(1:3)
#> # A tibble: 30 × 7
#> # Groups:   Resample [10]
#>    obs   pred     VF      F       M          L Resample
#>    <fct> <fct> <dbl>  <dbl>   <dbl>      <dbl> <chr>   
#>  1 VF    VF    0.914 0.0779 0.00848 0.0000199  Fold01  
#>  2 VF    VF    0.938 0.0571 0.00482 0.0000101  Fold01  
#>  3 VF    VF    0.947 0.0495 0.00316 0.00000500 Fold01  
#>  4 VF    VF    0.941 0.0544 0.00441 0.0000123  Fold02  
#>  5 VF    VF    0.948 0.0483 0.00347 0.00000792 Fold02  
#>  6 VF    VF    0.958 0.0395 0.00236 0.00000310 Fold02  
#>  7 VF    VF    0.939 0.0556 0.00513 0.00000790 Fold03  
#>  8 VF    VF    0.928 0.0642 0.00777 0.0000148  Fold03  
#>  9 VF    VF    0.927 0.0653 0.00786 0.0000150  Fold03  
#> 10 VF    VF    0.949 0.0469 0.00398 0.00000935 Fold04  
#> # ℹ 20 more rows
```

1 metric, 1 resample

``` r
hpc_cv |>
  filter(Resample == "Fold01") |>
  accuracy(obs, pred)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy multiclass     0.726
```

1 metric, 10 resamples

``` r
hpc_cv |>
  group_by(Resample) |>
  accuracy(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
```

2 metrics, 10 resamples

``` r
class_metrics <- metric_set(accuracy, kap)

hpc_cv |>
  group_by(Resample) |>
  class_metrics(obs, estimate = pred)
#> # A tibble: 20 × 4
#>    Resample .metric  .estimator .estimate
#>    <chr>    <chr>    <chr>          <dbl>
#>  1 Fold01   accuracy multiclass     0.726
#>  2 Fold02   accuracy multiclass     0.712
#>  3 Fold03   accuracy multiclass     0.758
#>  4 Fold04   accuracy multiclass     0.712
#>  5 Fold05   accuracy multiclass     0.712
#>  6 Fold06   accuracy multiclass     0.697
#>  7 Fold07   accuracy multiclass     0.675
#>  8 Fold08   accuracy multiclass     0.721
#>  9 Fold09   accuracy multiclass     0.673
#> 10 Fold10   accuracy multiclass     0.699
#> 11 Fold01   kap      multiclass     0.533
#> 12 Fold02   kap      multiclass     0.512
#> 13 Fold03   kap      multiclass     0.594
#> 14 Fold04   kap      multiclass     0.511
#> 15 Fold05   kap      multiclass     0.514
#> 16 Fold06   kap      multiclass     0.486
#> 17 Fold07   kap      multiclass     0.454
#> 18 Fold08   kap      multiclass     0.531
#> 19 Fold09   kap      multiclass     0.454
#> 20 Fold10   kap      multiclass     0.492
```

## Metrics

Below is a table of all of the metrics available in `yardstick`, grouped
by type.

| type                      | metric                                                                                                       |
|:--------------------------|:-------------------------------------------------------------------------------------------------------------|
| class                     | [`accuracy()`](https://yardstick.tidymodels.org/dev/reference/accuracy.md)                                   |
| class                     | [`bal_accuracy()`](https://yardstick.tidymodels.org/dev/reference/bal_accuracy.md)                           |
| class                     | [`detection_prevalence()`](https://yardstick.tidymodels.org/dev/reference/detection_prevalence.md)           |
| class                     | [`f_meas()`](https://yardstick.tidymodels.org/dev/reference/f_meas.md)                                       |
| class                     | [`fall_out()`](https://yardstick.tidymodels.org/dev/reference/fall_out.md)                                   |
| class                     | [`j_index()`](https://yardstick.tidymodels.org/dev/reference/j_index.md)                                     |
| class                     | [`kap()`](https://yardstick.tidymodels.org/dev/reference/kap.md)                                             |
| class                     | [`mcc()`](https://yardstick.tidymodels.org/dev/reference/mcc.md)                                             |
| class                     | [`miss_rate()`](https://yardstick.tidymodels.org/dev/reference/miss_rate.md)                                 |
| class                     | [`npv()`](https://yardstick.tidymodels.org/dev/reference/npv.md)                                             |
| class                     | [`ppv()`](https://yardstick.tidymodels.org/dev/reference/ppv.md)                                             |
| class                     | [`precision()`](https://yardstick.tidymodels.org/dev/reference/precision.md)                                 |
| class                     | [`recall()`](https://yardstick.tidymodels.org/dev/reference/recall.md)                                       |
| class                     | [`sens()`](https://yardstick.tidymodels.org/dev/reference/sens.md)                                           |
| class                     | [`sensitivity()`](https://yardstick.tidymodels.org/dev/reference/sens.md)                                    |
| class                     | [`spec()`](https://yardstick.tidymodels.org/dev/reference/spec.md)                                           |
| class                     | [`specificity()`](https://yardstick.tidymodels.org/dev/reference/spec.md)                                    |
| class prob                | [`average_precision()`](https://yardstick.tidymodels.org/dev/reference/average_precision.md)                 |
| class prob                | [`brier_class()`](https://yardstick.tidymodels.org/dev/reference/brier_class.md)                             |
| class prob                | [`classification_cost()`](https://yardstick.tidymodels.org/dev/reference/classification_cost.md)             |
| class prob                | [`gain_capture()`](https://yardstick.tidymodels.org/dev/reference/gain_capture.md)                           |
| class prob                | [`mn_log_loss()`](https://yardstick.tidymodels.org/dev/reference/mn_log_loss.md)                             |
| class prob                | [`pr_auc()`](https://yardstick.tidymodels.org/dev/reference/pr_auc.md)                                       |
| class prob                | [`roc_auc()`](https://yardstick.tidymodels.org/dev/reference/roc_auc.md)                                     |
| class prob                | [`roc_aunp()`](https://yardstick.tidymodels.org/dev/reference/roc_aunp.md)                                   |
| class prob                | [`roc_aunu()`](https://yardstick.tidymodels.org/dev/reference/roc_aunu.md)                                   |
| ordered prob              | [`ranked_prob_score()`](https://yardstick.tidymodels.org/dev/reference/ranked_prob_score.md)                 |
| numeric                   | [`ccc()`](https://yardstick.tidymodels.org/dev/reference/ccc.md)                                             |
| numeric                   | [`huber_loss()`](https://yardstick.tidymodels.org/dev/reference/huber_loss.md)                               |
| numeric                   | [`huber_loss_pseudo()`](https://yardstick.tidymodels.org/dev/reference/huber_loss_pseudo.md)                 |
| numeric                   | [`iic()`](https://yardstick.tidymodels.org/dev/reference/iic.md)                                             |
| numeric                   | [`mae()`](https://yardstick.tidymodels.org/dev/reference/mae.md)                                             |
| numeric                   | [`mape()`](https://yardstick.tidymodels.org/dev/reference/mape.md)                                           |
| numeric                   | [`mase()`](https://yardstick.tidymodels.org/dev/reference/mase.md)                                           |
| numeric                   | [`mpe()`](https://yardstick.tidymodels.org/dev/reference/mpe.md)                                             |
| numeric                   | [`msd()`](https://yardstick.tidymodels.org/dev/reference/msd.md)                                             |
| numeric                   | [`mse()`](https://yardstick.tidymodels.org/dev/reference/mse.md)                                             |
| numeric                   | [`poisson_log_loss()`](https://yardstick.tidymodels.org/dev/reference/poisson_log_loss.md)                   |
| numeric                   | [`rmse()`](https://yardstick.tidymodels.org/dev/reference/rmse.md)                                           |
| numeric                   | [`rmse_relative()`](https://yardstick.tidymodels.org/dev/reference/rmse_relative.md)                         |
| numeric                   | [`rpd()`](https://yardstick.tidymodels.org/dev/reference/rpd.md)                                             |
| numeric                   | [`rpiq()`](https://yardstick.tidymodels.org/dev/reference/rpiq.md)                                           |
| numeric                   | [`rsq()`](https://yardstick.tidymodels.org/dev/reference/rsq.md)                                             |
| numeric                   | [`rsq_trad()`](https://yardstick.tidymodels.org/dev/reference/rsq_trad.md)                                   |
| numeric                   | [`smape()`](https://yardstick.tidymodels.org/dev/reference/smape.md)                                         |
| dynamic survival          | [`brier_survival()`](https://yardstick.tidymodels.org/dev/reference/brier_survival.md)                       |
| dynamic survival          | [`roc_auc_survival()`](https://yardstick.tidymodels.org/dev/reference/roc_auc_survival.md)                   |
| integrated survival       | [`brier_survival_integrated()`](https://yardstick.tidymodels.org/dev/reference/brier_survival_integrated.md) |
| static survival           | [`concordance_survival()`](https://yardstick.tidymodels.org/dev/reference/concordance_survival.md)           |
| linear predictor survival | [`royston_survival()`](https://yardstick.tidymodels.org/dev/reference/royston_survival.md)                   |
| quantile                  | [`weighted_interval_score()`](https://yardstick.tidymodels.org/dev/reference/weighted_interval_score.md)     |
