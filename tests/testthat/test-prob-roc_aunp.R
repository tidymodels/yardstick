context("AUNP")

library(dplyr)

# HPC_CV takes too long
hpc_cv2 <- filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

soybeans <- data_soybean()

measures_mlr <- 0.964025841424236

test_that("AUNP is equivalent to macro_weighted estimator", {
  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    roc_aunp(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNP errors on binary case", {
  expect_error(
    roc_aunp(two_class_example, truth, Class1),
    "The number of levels in"
  )
})

test_that("AUNP results match mlr for soybean example", {
  expect_equal(
    roc_aunp(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[".estimate"]],
    measures_mlr
  )
})

