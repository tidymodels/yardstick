test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(path_tbl)[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(pathology, pathology, scan)[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    j_index_vec(df$pathology, df$scan),
    j_index_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    j_index(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(j_index_binary)
  )
  expect_equal(
    j_index(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(j_index_binary)
  )
  expect_equal(
    j_index(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp) / sum(p) + sum(tn) / sum(n) - 1)
  )
})

# ------------------------------------------------------------------------------

test_that("two class with case weights is correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 10L, 2L)
  )

  expect_identical(
    j_index(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    -10/11
  )
})
