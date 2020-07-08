# Internal helper to query a default `event_level`
#
# 1) Respect `yardstick.event_first` if set, but warn about deprecation
# 2) Return `"first"` otherwise as the default event level
#
# Metric functions that use this helper can completely ignore the global option
# by setting the `event_first` argument to `"first"` or `"second"` directly.
yardstick_event_level <- function() {
  opt <- getOption("yardstick.event_first")

  if (is.null(opt)) {
    return("first")
  }

  warn_event_first_deprecated()

  if (identical(opt, TRUE)) {
    "first"
  } else if (identical(opt, FALSE)) {
    "second"
  } else {
    abort("Global option `yardstick.event_first` is set, but is not `TRUE` or `FALSE`.")
  }
}

warn_event_first_deprecated <- function() {
  msg <- paste0(
    "The `yardstick.event_first` option has been deprecated as of ",
    "yardstick 0.0.7 and will completely ignored in a future version.\n",
    "Instead, set the following argument directly in the metric function:\n",
    "`yardstick.event_first = TRUE`  -> `event_level = 'first'` (the default)\n",
    "`yardstick.event_first = FALSE` -> `event_level = 'second'`"
  )

  rlang::warn(msg, class = "yardstick_warning_event_first_deprecated")
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

  rlang::abort("`event_level` must be 'first' or 'second'.")
}
