test_that("yardstick_remove_missing works", {
  expect_identical(
    yardstick_remove_missing(1:10, 1:10, 1:10),
    list(
      truth = 1:10,
      estimate = 1:10,
      case_weights = 1:10,
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(c(1:4, NA, NA, 7:10), 1:10, 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10),
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, c(1:4, NA, NA, 7:10), 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10),
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, 1:10, c(1:4, NA, NA, 7:10)),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10),
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(1:10, c(1:4, NA, NA, 7:10), 1:10),
    list(
      truth = c(1:4, 7:10),
      estimate = c(1:4, 7:10),
      case_weights = c(1:4, 7:10),
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(c(NA, 2:10), c(1:9, NA), c(1:4, NA, NA, 7:10)),
    list(
      truth = c(2:4, 7:9),
      estimate = c(2:4, 7:9),
      case_weights = c(2:4, 7:9),
      censoring_weights = NULL,
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(
      c(NA, 2:10), c(1:9, NA), c(1:4, NA, NA, 7:10), c(NA, NA, 3:10)
    ),
    list(
      truth = c(3:4, 7:9),
      estimate = c(3:4, 7:9),
      case_weights = c(3:4, 7:9),
      censoring_weights = c(3:4, 7:9),
      eval_times = NULL
    )
  )

  expect_identical(
    yardstick_remove_missing(
      truth = c(NA, 2:10),
      estimate = c(1:9, NA),
      case_weights = c(1:4, NA, NA, 7:10),
      censoring_weights = c(NA, NA, 3:10),
      eval_times = c(1:2, NA, 4:10)
    ),
    list(
      truth = c(4L, 7:9),
      estimate = c(4L, 7:9),
      case_weights = c(4L, 7:9),
      censoring_weights = c(4L, 7:9),
      eval_times = c(4L, 7:9)
    )
  )
})

test_that("yardstick_any_missing works", {
  expect_false(
    yardstick_any_missing(1:10, 1:10, 1:10)
  )

  expect_false(
    yardstick_any_missing(1:10, 1:10, 1:10, 1:10)
  )

  expect_false(
    yardstick_any_missing(1:10, 1:10, 1:10, 1:10, 1:10)
  )

  expect_true(
    yardstick_any_missing(c(1:4, NA, NA, 7:10), 1:10, 1:10)
  )

  expect_true(
    yardstick_any_missing(1:10, c(1:4, NA, NA, 7:10), 1:10)
  )

  expect_true(
    yardstick_any_missing(1:10, 1:10, c(1:4, NA, NA, 7:10))
  )

  expect_true(
    yardstick_any_missing(1:10, c(1:4, NA, NA, 7:10), 1:10)
  )

  expect_true(
    yardstick_any_missing(c(NA, 2:10), c(1:9, NA), c(1:4, NA, NA, 7:10))
  )

  expect_true(
    yardstick_any_missing(1:10, 1:10, 1:10, c(NA, 2:10))
  )

  expect_true(
    yardstick_any_missing(1:10, 1:10, 1:10, 1:10, c(NA, 2:10))
  )
})
