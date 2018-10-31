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
