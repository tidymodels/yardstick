# `yardstick_event_level()` errors when option is set - TRUE

    Code
      yardstick_event_level()
    Condition
      Error:
      ! The global option `yardstick.event_first` was deprecated in yardstick 0.0.7 and is now defunct.
      i Please use the metric function argument `event_level` instead.
      i The global option is being ignored entirely.

# `yardstick_event_level()` errors when option is set - FALSE

    Code
      yardstick_event_level()
    Condition
      Error:
      ! The global option `yardstick.event_first` was deprecated in yardstick 0.0.7 and is now defunct.
      i Please use the metric function argument `event_level` instead.
      i The global option is being ignored entirely.

# validate_event_level() works

    Code
      recall(two_class_example, truth, predicted, event_level = "wrong")
    Condition
      Error in `recall()`:
      ! `event_level` must be "first" or "second".

