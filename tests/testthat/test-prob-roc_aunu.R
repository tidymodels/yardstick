context("AUNU")

library(dplyr)

# HPC_CV takes too long
hpc_cv2 <- filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

soybeans <- data_soybean()

measures_mlr <- 0.963473055084008

test_that("AUNU is equivalent to macro estimator", {
  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    roc_aunu(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNU errors on binary case", {
  expect_error(
    roc_aunu(two_class_example, truth, Class1),
    "The number of levels in"
  )
})

test_that("AUNU results match mlr for soybean example", {
  expect_equal(
    roc_aunu(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[".estimate"]],
    measures_mlr
  )
})

