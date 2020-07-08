test_that("`yardstick_event_level()` defaults to 'first'", {
  expect_identical(yardstick_event_level(), "first")
})

test_that("`yardstick_event_level()` respects option - TRUE, with a warning", {
  local_lifecycle_warnings()
  rlang::local_options(yardstick.event_first = TRUE)
  expect_identical(expect_warning(yardstick_event_level()), "first")
})

test_that("`yardstick_event_level()` respects option - FALSE, with a warning", {
  local_lifecycle_warnings()
  rlang::local_options(yardstick.event_first = FALSE)
  expect_identical(expect_warning(yardstick_event_level()), "second")
})

