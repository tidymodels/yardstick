library(survival) # survival_3.5-3
library(riskRegression) # riskRegression_2023.03.10
library(prodlim) # prodlim_2022.10.13
library(modeldata)

# ------------------------------------------------------------------------------

data(wa_churn)

wa_churn <-
  wa_churn |>
  filter(!is.na(total_charges)) |>
  mutate(
    status = ifelse(churn == "No", 1, 0)
  ) |>
  select(tenure, status, female, total_charges)

# ------------------------------------------------------------------------------

cox_fit <- coxph(
  Surv(tenure, status) ~ female + total_charges,
  data = wa_churn,
  y = TRUE,
  x = TRUE
)

# ------------------------------------------------------------------------------

xs_auc <- Score(
  list("churn" = cox_fit),
  formula = Surv(tenure, status) ~ 1,
  data = wa_churn,
  conf.int = FALSE,
  times = c(1, 23, 70),
  metrics = "AUC",
  cens.method = "ipcw",
  cens.model = "km",
  seed = 1
)

xs_brier <- Score(
  list("churn" = cox_fit),
  formula = Surv(tenure, status) ~ 1,
  data = wa_churn,
  conf.int = FALSE,
  times = c(1, 23, 70),
  metrics = "Brier",
  cens.method = "ipcw",
  cens.model = "km",
  seed = 1
)

# ------------------------------------------------------------------------------

# after getPerformanceData()
if (FALSE) {
  rr_churn_data <- as.data.frame(DT)
  save(rr_churn_data, file = "rr_churn_data.RData")
}

# after computePerformance() when metrics = "AUC"
if (FALSE) {
  auc_churn_res <- as.data.frame(noSplit$AUC$score)
  save(auc_churn_res, file = "auc_churn_res.RData")
}

# after computePerformance() when metrics = "Brier"
if (FALSE) {
  brier_churn_res <- as.data.frame(noSplit$Brier$score)
  save(brier_churn_res, file = "brier_churn_res.RData")
}
