context("Global option: yardstick.event_first")

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

###################################################################

test_that('starts true', {
  expect_true("yardstick.event_first" %in% names(options()))
  expect_true(getOption("yardstick.event_first"))
})

test_that('Can flip global option', {
  options(yardstick.event_first = FALSE)
  on.exit(options(yardstick.event_first = TRUE))
  expect_false(getOption("yardstick.event_first"))
})

###################################################################

test_that('switch event definition', {
  options(yardstick.event_first = FALSE)
  on.exit(options(yardstick.event_first = TRUE))

  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54/86
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231/258
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    231/258
  )
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
})


test_that('global option is ignored in multiclass metrics', {

  on.exit(options(yardstick.event_first = TRUE))

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      accuracy(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      accuracy(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      bal_accuracy(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      bal_accuracy(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      detection_prevalence(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      detection_prevalence(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      f_meas(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      f_meas(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      j_index(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      j_index(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      kap(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      kap(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      mcc(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      mcc(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      npv(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      npv(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      ppv(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      ppv(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      precision(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      precision(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      recall(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      recall(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      sens(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      sens(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      spec(hpc_cv, obs, pred)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      spec(hpc_cv, obs, pred)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      gain_capture(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      gain_capture(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      gain_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      gain_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      lift_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      lift_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      mn_log_loss(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      mn_log_loss(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      pr_auc(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      pr_auc(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      pr_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      pr_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  # testing hand till
  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      roc_auc(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      roc_auc(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

  # testing macro
  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      roc_auc(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      roc_auc(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]]
    }
  )

  expect_equal(
    {
      options(yardstick.event_first = TRUE)
      roc_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    },
    {
      options(yardstick.event_first = FALSE)
      roc_curve(hpc_cv, obs, VF:L)[[".estimate"]]
    }
  )

})
