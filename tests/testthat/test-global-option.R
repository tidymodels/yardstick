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
