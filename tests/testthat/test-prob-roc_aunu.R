test_that("AUNU is equivalent to macro estimator", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    roc_aunu(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNU errors on binary case", {
  expect_snapshot((expect_error(
    roc_aunu(two_class_example, truth, Class1)
  )))
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

# ------------------------------------------------------------------------------

test_that("roc_aunu() - `options` is deprecated", {
  skip_if(getRversion() <= "3.5.3", "Base R used a different deprecated warning class.")
  local_lifecycle_warnings()

  expect_snapshot({
    out <- roc_aunu(two_class_example, truth, Class1, Class2, options = 1)
  })

  expect_identical(
    out,
    roc_aunu(two_class_example, truth, Class1, Class2),
  )

  expect_snapshot({
    out <- roc_aunu_vec(
      truth = two_class_example$truth,
      estimate = as.matrix(two_class_example[c("Class1", "Class2")]),
      options = 1
    )
  })

  expect_identical(
    out,
    roc_aunu_vec(
      truth = two_class_example$truth,
      estimate = as.matrix(two_class_example[c("Class1", "Class2")])
    )
  )
})
