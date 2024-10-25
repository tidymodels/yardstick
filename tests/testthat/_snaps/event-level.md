# `yardstick_event_level()` ignores option - TRUE, with a warning

    Code
      out <- yardstick_event_level()
    Condition
      Warning:
      The global option `yardstick.event_first` was deprecated in yardstick 0.0.7.
      i Please use the metric function argument `event_level` instead.
      i The global option is being ignored entirely.

# `yardstick_event_level()` ignores option - FALSE, with a warning

    Code
      out <- yardstick_event_level()
    Condition
      Warning:
      The global option `yardstick.event_first` was deprecated in yardstick 0.0.7.
      i Please use the metric function argument `event_level` instead.
      i The global option is being ignored entirely.

# validate_event_level() works

    Code
      recall(two_class_example, truth, predicted, event_level = "wrong")
    Condition
      Error in `recall()`:
      ! `event_level` must be "first" or "second".

