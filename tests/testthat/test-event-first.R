test_that("`is_event_first()` works when ignoring the option", {
  expect_true(is_event_first("binary", check_option = FALSE))
  expect_false(is_event_first("binary-last", check_option = FALSE))
})

test_that("multiclass `estimator` is not allowed", {
  expect_error(is_event_first("macro"), "Multiclass estimator")
})

test_that("`'binary-last'` overrides the option", {
  rlang::local_options(yardstick.event_first = TRUE)

  expect_warning(
    expect_false(is_event_first("binary-last")),
    "The `yardstick.event_first` option has been deprecated"
  )
})

test_that("`'binary'` respects the option", {
  rlang::local_options(yardstick.event_first = FALSE)

  expect_warning(
    expect_false(is_event_first("binary")),
    "The `yardstick.event_first` option has been deprecated"
  )
})
