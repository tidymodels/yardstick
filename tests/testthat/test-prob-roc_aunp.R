context("AUNP")

test_that("AUNP is equivalent to macro_weighted estimator", {
  hpc_f1 <- data_hpc_fold1()

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
  soybeans <- data_soybean()

  # Code to generate this value and `data_soybean()` is in `helper-data.R`
  measures_mlr <- 0.964025841424236

  expect_equal(
    roc_aunp(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[".estimate"]],
    measures_mlr
  )
})

