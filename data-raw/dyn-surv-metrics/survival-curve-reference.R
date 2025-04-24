tidy_churn <- readRDS(test_path("data/tidy_churn.rds"))

ref_roc_auc_survival <- tidy_churn |>
  roc_auc_survival(
    truth = surv_obj,
    .pred
  )

yardstick_res <- ref_roc_auc_survival |>
  readr::write_rds("tests/testthat/data/ref_roc_auc_survival.rds")

ref_roc_curve_survival <- tidy_churn |>
  roc_curve_survival(
    truth = surv_obj,
    .pred
  )

yardstick_res <- ref_roc_curve_survival |>
  readr::write_rds("tests/testthat/data/ref_roc_curve_survival.rds")
