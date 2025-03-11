lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

###################################################################

test_that("switch event definition", {
  rlang::local_options(
    yardstick.event_first = FALSE,
    lifecycle_verbosity = "quiet"
  )

  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    77 / 86
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    77 / 86
  )
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    162 / 258
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    162 / 258
  )
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (162 / 258) + (77 / 86) - 1
  )
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) /
      sqrt((231 + 32) * (231 + 27) * (54 + 32) * (54 + 27))
  )
})

test_that("global option is ignored in multiclass metrics", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    rlang::with_options(
      accuracy(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      accuracy(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      bal_accuracy(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      bal_accuracy(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      detection_prevalence(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      detection_prevalence(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      f_meas(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      f_meas(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      j_index(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      j_index(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      kap(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      kap(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      mcc(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      mcc(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      npv(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      npv(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      ppv(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      ppv(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      precision(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      precision(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      recall(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      recall(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      sens(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      sens(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      spec(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      spec(hpc_cv, obs, pred)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      gain_capture(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      gain_capture(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      gain_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      gain_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      lift_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      lift_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      mn_log_loss(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      mn_log_loss(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      pr_auc(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      pr_auc(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      pr_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      pr_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  # testing hand till
  expect_equal(
    rlang::with_options(
      roc_auc(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      roc_auc(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  # testing macro
  expect_equal(
    rlang::with_options(
      roc_auc(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      roc_auc(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )

  expect_equal(
    rlang::with_options(
      roc_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = TRUE
    ),
    rlang::with_options(
      roc_curve(hpc_cv, obs, VF:L)[[".estimate"]],
      yardstick.event_first = FALSE
    )
  )
})
