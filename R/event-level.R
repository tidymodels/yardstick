# Internal helper to query a default `event_level`
#
# 1) Respect `yardstick.event_first` if set, but warn about deprecation
# 2) Return `"first"` otherwise as the default event level
#
# Metric functions that use this helper can completely ignore the global option
# by setting the `event_first` argument to `"first"` or `"second"` directly.
yardstick_event_level <- function() {
  opt <- getOption("yardstick.event_first")

  if (!is.null(opt)) {
    lifecycle::deprecate_warn(
      when = "0.0.7",
      what = I("The global option `yardstick.event_first`"),
      with = I("the metric function argument `event_level`"),
      details = "The global option is being ignored entirely."
    )
  }

  "first"
}

is_event_first <- function(event_level) {
  validate_event_level(event_level)
  identical(event_level, "first")
}

validate_event_level <- function(event_level) {
  if (identical(event_level, "first")) {
    return(invisible())
  }
  if (identical(event_level, "second")) {
    return(invisible())
  }

  cli::cli_abort("{.arg event_level} must be {.val first} or {.val second}.")
}
