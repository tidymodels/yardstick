test_that("`yardstick_event_level()` defaults to 'first'", {
  expect_identical(yardstick_event_level(), "first")
})

test_that("`yardstick_event_level()` errors when option is set - TRUE", {
  rlang::local_options(yardstick.event_first = TRUE)
  expect_snapshot(error = TRUE, yardstick_event_level())
})

test_that("`yardstick_event_level()` errors when option is set - FALSE", {
  rlang::local_options(yardstick.event_first = FALSE)
  expect_snapshot(error = TRUE, yardstick_event_level())
})

test_that("validate_event_level() works", {
  expect_snapshot(
    error = TRUE,
    recall(two_class_example, truth, predicted, event_level = "wrong")
  )
})
