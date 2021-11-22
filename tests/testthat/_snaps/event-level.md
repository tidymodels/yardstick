# `yardstick_event_level()` respects option - TRUE, with a warning

    Code
      out <- yardstick_event_level()
    Warning <deprecatedWarning>
      The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
      Instead, set the following argument directly in the metric function:
      `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
      `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`

# `yardstick_event_level()` respects option - FALSE, with a warning

    Code
      out <- yardstick_event_level()
    Warning <deprecatedWarning>
      The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
      Instead, set the following argument directly in the metric function:
      `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
      `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`

