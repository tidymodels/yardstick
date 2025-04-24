library(tidymodels)
library(censored)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

lung_data <-
  survival::lung |>
  select(time, status, age, sex, ph.ecog)

model_fit <-
  survival_reg() |>
  fit(Surv(time, status) ~ age + sex + ph.ecog, data = lung_data)

# ------------------------------------------------------------------------------

pred_times <- (1:5) * 100

# Data to compute metrics:
lung_surv <-
  # Now dynamic predictions at 5 time points
  predict(model_fit, lung_data, type = "survival", eval_time = pred_times) |>
  bind_cols(
    # Static predictions
    predict(model_fit, lung_data, type = "time"),
    # We'll need the surv object
    lung_data |> transmute(surv_obj = Surv(time, status))
  ) |>
  .censoring_weights_graf(model_fit, .)

usethis::use_data(lung_surv, overwrite = TRUE)
