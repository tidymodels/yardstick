context("J Index")

# ------------------------------------------------------------------------------

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

test_that("`estimator = 'binary_last'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    j_index_vec(df$pathology, df$scan),
    j_index_vec(df_rev$pathology, df_rev$scan, estimator = "binary_last")
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
