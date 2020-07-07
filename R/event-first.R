opt_event_first <- function() {
  opt <- getOption("yardstick.event_first", default = TRUE)
  identical(opt, TRUE)
}

is_event_first <- function(estimator, check_option = TRUE) {
  if (check_option) {
    opt <- getOption("yardstick.event_first")

    if (is.null(opt)) {
      opt <- TRUE
    } else {
      warn_event_first_deprecated()
      opt <- identical(opt, TRUE)
    }
  }

  # Setting `estimator = 'binary_last'` overrides option
  if (identical(estimator, "binary_last")) {
    return(FALSE)
  }

  if (!identical(estimator, "binary")) {
    rlang::abort("Internal error: Multiclass estimator should never check `is_event_first()`.")
  }

  if (check_option) {
    return(opt)
  }

  TRUE
}


warn_event_first_deprecated <- function() {
  msg <- paste0(
    "The `yardstick.event_first` option has been deprecated as of ",
    "yardstick 0.0.7 and will completely ignored in a future version.\n",
    "Instead, set the following argument directly in the metric function:\n",
    "`yardstick.event_first = TRUE`  -> `estimator = 'binary'` (the default)\n",
    "`yardstick.event_first = FALSE` -> `estimator = 'binary_last'`"
  )

  rlang::warn(msg, class = "yardstick_warning_event_first_deprecated")
}
