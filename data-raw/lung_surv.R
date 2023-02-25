library(tidymodels)
library(censored)

data(cancer)

lung <- lung %>% drop_na()
lung_train <- lung[-c(1:50), ]
lung_test <- lung[1:50, ]

sr_spec <-
  survival_reg(dist = "weibull") %>%
  set_engine("survival") %>%
  set_mode("censored regression")

set.seed(1)
sr_fit <- sr_spec %>% fit(Surv(time, status) ~ ., data = lung_train)

censor_probs <- function(x) {
  # JFC prodlim check for the right class by inspecting the call :-O
  dat <- data.frame(time = x[, "time"], status = x[, "status"])
  # TODO check for 2+ censored values
  kn_cens <- prodlim::prodlim(survival::Surv(time, status) ~ 1, dat, reverse = TRUE)

  cen_times <- as.data.frame(kn_cens[c("time", "n.lost", "surv")])
  cen_times <- cen_times[cen_times$n.lost > 0, -2]
  cen_times$surv <- 1 - cen_times$surv
  colnames(cen_times) <- c("time", "prob_censored")

  bounds <- dplyr::tibble(time = c(0, Inf), prob_censored = c(0, 1))
  cen_times <- dplyr::bind_rows(bounds, cen_times)
  dplyr::arrange(cen_times, time)
}

get_single_censor_prob <- function(x, probs) {
  probs$prob_censored[max(which(x > probs$time))]
}

censor_dist <- censor_probs(Surv(lung_test$time, lung_test$status))

lung_surv <- lung_test %>%
  as_tibble() %>%
  mutate(surv_obj = Surv(time, status)) %>%
  bind_cols(predict(
    sr_fit,
    lung_test,
    type = "survival",
    time = c(100, 500, 1000)
  )) %>%
  unnest(.pred) %>%
  rename(eval_time = .time) %>%
  mutate(
    prob_censored = if_else(
      time < eval_time & status == 2,
      1/(1 - map_dbl(time, get_single_censor_prob, censor_dist)),
      1/(1 - map_dbl(eval_time, get_single_censor_prob, censor_dist)),
    )
  ) %>%
  relocate(surv_obj, eval_time, .pred_survival, prob_censored)

usethis::use_data(lung_surv, overwrite = TRUE)
