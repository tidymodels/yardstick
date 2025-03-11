test_that("`yardstick_event_level()` defaults to 'first'", {
  expect_identical(yardstick_event_level(), "first")
})

test_that("`yardstick_event_level()` ignores option - TRUE, with a warning", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")
  rlang::local_options(yardstick.event_first = TRUE)
  expect_snapshot(out <- yardstick_event_level())
  expect_identical(out, "first")
})

test_that("`yardstick_event_level()` ignores option - FALSE, with a warning", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")
  rlang::local_options(yardstick.event_first = FALSE)
  expect_snapshot(out <- yardstick_event_level())
  expect_identical(out, "first")
})

test_that("validate_event_level() works", {
  expect_snapshot(
    error = TRUE,
    recall(two_class_example, truth, predicted, event_level = "wrong")
  )
})
