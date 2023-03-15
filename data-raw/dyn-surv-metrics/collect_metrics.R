# yardstick_1.1.0.9001
# pak::pak(c("tidymodels/yardstick#290"), ask = FALSE)

library(tidymodels)
library(survival)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

# Objects saved from riskRegression::Score() via browser()
load("rr_churn_data.RData")
load("brier_churn_res.RData")
load("auc_churn_res.RData")

# ------------------------------------------------------------------------------

rr_churn_data <- rr_churn_data[rr_churn_data$model == 1,]
rr_churn_data$surv <- Surv(rr_churn_data$time, rr_churn_data$status)
rr_churn_data$surv_prob <- 1 - rr_churn_data$risk

# From Brier.survival()
# DT[time<=times & status==1,residuals:=(1-risk)^2/WTi]
# DT[time<=times & status==0,residuals:=0]
# DT[time>times,residuals:=(risk)^2/Wt]

g_1 <- rr_churn_data$time <= rr_churn_data$times & rr_churn_data$status == 1
g_2 <- rr_churn_data$time <= rr_churn_data$times & rr_churn_data$status == 0
g_3 <- rr_churn_data$time > rr_churn_data$times

rr_churn_data$ipcw <- NA_real_
rr_churn_data$ipcw[g_1] <- 1 / rr_churn_data$WTi[g_1]
rr_churn_data$ipcw[g_2] <- 0
rr_churn_data$ipcw[g_3] <- 1 / rr_churn_data$Wt[g_3]

# ------------------------------------------------------------------------------
# check Brier results

rr_churn_data %>% 
  brier_survival(
    surv,
    estimate = surv_prob,
    censoring_weights = ipcw,
    eval_time = times
  )
brier_churn_res %>% filter(grepl("churn",  model))

# ------------------------------------------------------------------------------
# check ROC results

rr_churn_data %>% 
  group_by(times) %>% 
  roc_auc_survival(
    surv,
    estimate = surv_prob,
    censoring_weights = ipcw,
    eval_time = times
  )
auc_churn_res %>% filter(grepl("churn",  model))

# ------------------------------------------------------------------------------

surv_metric_expected <- 
  bind_rows(
    auc_churn_res %>% 
      filter(grepl("churn",  model)) %>% 
      select(.estimate = AUC, eval_time = times) %>% 
      mutate(.metric = "roc_auc_survival"),
    brier_churn_res %>% 
      filter(grepl("churn",  model)) %>% 
      select(.estimate = Brier, eval_time = times) %>% 
      mutate(.metric = "brier_survival")
  ) %>% 
  as_tibble()

save(surv_metric_expected,
     rr_churn_data,
     file = "surv_metric_expected.RData",
     compress = TRUE)


