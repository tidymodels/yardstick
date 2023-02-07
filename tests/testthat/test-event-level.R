test_that("`yardstick_event_level()` defaults to 'first'", {
  expect_identical(yardstick_event_level(), "first")
})

test_that("`yardstick_event_level()` ignores option - TRUE, with a warning", {
  skip_if(getRversion() <= "3.5.3", "Base R used a different deprecated warning class.")
  local_lifecycle_warnings()
  rlang::local_options(yardstick.event_first = TRUE)
  expect_snapshot(out <- yardstick_event_level())
  expect_identical(out, "first")
})

test_that("`yardstick_event_level()` ignores option - FALSE, with a warning", {
  skip_if(getRversion() <= "3.5.3", "Base R used a different deprecated warning class.")
  local_lifecycle_warnings()
  rlang::local_options(yardstick.event_first = FALSE)
  expect_snapshot(out <- yardstick_event_level())
  expect_identical(out, "first")
})

