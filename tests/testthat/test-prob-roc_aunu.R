context("AUNU")

test_that("AUNU is equivalent to macro estimator", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    roc_aunu(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNU errors on binary case", {
  expect_error(
    roc_aunu(two_class_example, truth, Class1),
    "The number of levels in",
    class = "dplyr_error"
  )
})

test_that("AUNU results match mlr for soybean example", {
  soybeans <- data_soybean()

  # Code to generate this value and `data_soybean()` is in `helper-data.R`
  measures_mlr <- 0.963473055084008

  expect_equal(
    roc_aunu(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[".estimate"]],
    measures_mlr
  )
})

