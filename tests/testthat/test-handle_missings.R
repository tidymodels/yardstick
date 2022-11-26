test_that("yardstick_remove_missing works", {
  expect_identical(
    yardstick_remove_missing(1:10, 1:10, 1:10),
    list(
      truth = 1:10,
      estimate = 1:10,
      case_weights = 1:10
    )
  )

  expect_identical(
    yardstick_remove_missing(c(1:4, NA, NA, 7:10), 1:10, 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10)
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, c(1:4, NA, NA, 7:10), 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10)
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, 1:10, c(1:4, NA, NA, 7:10)),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10)
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, c(1:4, NA, NA, 7:10), 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10)
    )
  )

  expect_identical(
    yardstick_remove_missing(c(NA, 2:10), c(1:9, NA), c(1:4, NA, NA, 7:10)),
    list(
      truth = c(2:4, 7:9),
      estimate = c(2:4, 7:9),
      case_weights = c(2:4, 7:9)
    )
  )
})

test_that("detect_missings works", {
  expect_false(
    detect_missings(1:10, 1:10, 1:10)
  )

  expect_true(
    detect_missings(c(1:4, NA, NA, 7:10), 1:10, 1:10)
  )

  expect_true(
    detect_missings(1:10, c(1:4, NA, NA, 7:10), 1:10)
  )

  expect_true(
    detect_missings(1:10, 1:10, c(1:4, NA, NA, 7:10))
  )

  expect_true(
    detect_missings(1:10, c(1:4, NA, NA, 7:10), 1:10)
  )

  expect_true(
    detect_missings(c(NA, 2:10), c(1:9, NA), c(1:4, NA, NA, 7:10))
  )
})
