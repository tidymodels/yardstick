context("MCC")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(path_tbl)[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(pathology, truth = pathology, estimate = scan_na)[[".estimate"]],
    ((230 * 53) - (32 * 26)) / sqrt((230 + 32)*(230 + 26) * (53 + 32) * (53 + 26))
  )
})

# sklearn compare --------------------------------------------------------------

py_res <- read_pydata("py-mcc")
r_metric <- mcc

test_that('Two class - sklearn equivalent', {
  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})
