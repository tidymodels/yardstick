test_that("AUNP is equivalent to macro_weighted estimator", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    roc_aunp(hpc_f1, obs, VF:L)[[".estimate"]]
  )
})

test_that("AUNP is equivalent to macro_weighted estimator with case weights", {
  hpc_cv$weight <- read_weights_hpc_cv()

  expect_equal(
    roc_auc(
      hpc_cv,
      obs,
      VF:L,
      estimator = "macro_weighted",
      case_weights = weight
    )[[".estimate"]],
    roc_aunp(hpc_cv, obs, VF:L, case_weights = weight)[[".estimate"]]
  )
})

test_that("AUNP errors on binary case", {
  expect_snapshot(
    error = TRUE,
    roc_aunp(two_class_example, truth, Class1)
  )
})

test_that("AUNP results match mlr for soybean example", {
  soybeans <- data_soybean()

  # Code to generate this value and `data_soybean()` is in `helper-data.R`
  measures_mlr <- 0.964025841424236

  expect_equal(
    roc_aunp(soybeans, truth, `2-4-d-injury`:`rhizoctonia-root-rot`)[[
      ".estimate"
    ]],
    measures_mlr
  )
})

# ------------------------------------------------------------------------------

test_that("roc_aunp() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_snapshot({
    out <- roc_aunp(two_class_example, truth, Class1, Class2, options = 1)
  })

  expect_identical(
    out,
    roc_aunp(two_class_example, truth, Class1, Class2)
  )

  expect_snapshot({
    out <- roc_aunp_vec(
      truth = two_class_example$truth,
      estimate = as.matrix(two_class_example[c("Class1", "Class2")]),
      options = 1
    )
  })

  expect_identical(
    out,
    roc_aunp_vec(
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
    roc_aunp_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    roc_aunp_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = freq_wgt
    )
  )
})

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- as.matrix(two_class_example[c("Class1", "Class2")])

  expect_snapshot(
    error = TRUE,
    roc_aunp_vec(cp_truth, estimate)
  )
})
