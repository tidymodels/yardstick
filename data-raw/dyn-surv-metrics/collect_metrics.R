library(tidymodels)
library(survival)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

# Objects saved from riskRegression::Score() via browser()
load("data-raw/dyn-surv-metrics/rr_churn_data.RData")
load("data-raw/dyn-surv-metrics/brier_churn_res.RData")
load("data-raw/dyn-surv-metrics/auc_churn_res.RData")

brier_churn_res |>
  filter(grepl("churn", model)) |>
  readr::write_rds("tests/testthat/data/brier_churn_res.rds")

auc_churn_res |>
  readr::write_rds("tests/testthat/data/auc_churn_res.rds")

# ------------------------------------------------------------------------------

rr_churn_data <- rr_churn_data[rr_churn_data$model == 1, ]
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

rr_churn_data |>
  readr::write_rds("tests/testthat/data/rr_churn_data.rds")

tidy_churn <- readRDS(test_path("data/rr_churn_data.rds")) |>
  dplyr::rename(
    .eval_time = times,
    .pred_survival = surv_prob,
    .weight_censored = ipcw
  ) |>
  dplyr::mutate(
    .weight_censored = dplyr::if_else(
      status == 0 & time < .eval_time,
      NA,
      .weight_censored
    )
  ) |>
  tidyr::nest(.pred = -c(ID, time, status, model)) |>
  dplyr::mutate(surv_obj = survival::Surv(time, status))

tidy_churn |>
  readr::write_rds("tests/testthat/data/tidy_churn.rds")
