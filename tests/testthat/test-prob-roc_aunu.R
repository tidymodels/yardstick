test_that("AUNU is equivalent to macro estimator", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    roc_aunu(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNU is equivalent to macro estimator with case weights", {
  hpc_cv$weight <- read_weights_hpc_cv()

  expect_equal(
    roc_auc(hpc_cv, obs, VF:L, estimator = "macro", case_weights = weight)[[
      ".estimate"
    ]],
    roc_aunu(hpc_cv, obs, VF:L, case_weights = weight)[[".estimate"]]
  )
})

test_that("AUNU errors on binary case", {
  expect_snapshot(
    error = TRUE,
    roc_aunu(two_class_example, truth, Class1)
  )
})

test_that("AUNU results match mlr for soybean example", {
  soybeans <- data_soybean()

  # Code to generate this value and `data_soybean()` is in `helper-data.R`
  measures_mlr <- 0.963473055084008

  expect_equal(
    roc_aunu(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[
      ".estimate"
    ]],
    measures_mlr
  )
})

# ------------------------------------------------------------------------------

test_that("roc_aunu() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

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

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    roc_aunu_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    roc_aunu_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = freq_wgt
    )
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- as.matrix(two_class_example[c("Class1", "Class2")])

  expect_snapshot(
    error = TRUE,
    roc_aunu_vec(cp_truth, estimate)
  )
})
