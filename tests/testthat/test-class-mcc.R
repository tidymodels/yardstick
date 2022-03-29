test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

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

test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    mcc_vec(df$pathology, df$scan),
    mcc_vec(df_rev$pathology, df_rev$scan)
  )
})

test_that("doesn't integer overflow (#108)", {
  x <- matrix(c(50122L, 50267L, 49707L, 49904L), ncol = 2L, nrow = 2L)
  expect_equal(
    mcc(x)[[".estimate"]],
    0.00026665430738672
  )
})

# sklearn compare --------------------------------------------------------------

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})
