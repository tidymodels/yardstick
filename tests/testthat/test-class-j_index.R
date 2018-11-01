context("J Index")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
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

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()
micro <- data_three_by_three_micro()

test_that('Three class', {

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
