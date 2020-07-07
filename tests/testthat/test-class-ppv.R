context("PPV")

# ------------------------------------------------------------------------------

test_that('ppv', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    ppv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(path_tbl)[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[".estimate"]],
    0.70642,
    tolerance = .001
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    ppv_vec(df$pathology, df$scan),
    ppv_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()
  micro$prev <- (micro$tp + micro$fn) / (micro$p + micro$n)

  expect_equal(
    ppv(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(ppv_binary)
  )
  expect_equal(
    ppv(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(ppv_binary)
  )
  # PPV = Precision when no prevalence is given
  expect_equal(
    ppv(multi_ex, estimator = "micro")[[".estimate"]],
    precision(multi_ex, estimator = "micro")[[".estimate"]]
  )
  expect_equal(
    ppv(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro,
         ((sum(tp) / sum(p)) * sum(prev)) /
           ( (sum(tp) / sum(p)) * sum(prev) + ((1 - sum(tn) / sum(n)) * sum((1 - prev))) )
    )
  )
  # Prevalence defined by the user. Defined once for all levels?
  expect_equal(
    ppv(multi_ex, estimator = "micro", prevalence = .4)[[".estimate"]],
    with(micro,
         ((sum(tp) / sum(p)) * sum(.4)) /
           ( (sum(tp) / sum(p)) * sum(.4) + ((1 - sum(tn) / sum(n)) * sum((1 - .4))) )
    )
  )
})
