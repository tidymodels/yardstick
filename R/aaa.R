## Taken form https://github.com/tidyverse/dplyr/blob/d310ad1cef1c14d770c94e1a9a4c79c888f46af6/R/zzz.r#L2-L9

# nocov start
# tested abyway in test_two_class.R
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.yardstick <- list(
    yardstick.event_first = TRUE
  )
  toset <- !(names(op.yardstick) %in% names(op))
  if (any(toset)) options(op.yardstick[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "For binary classification, the first factor level is assumed to ",
    "be the event.\nSet the global option `yardstick.event_first` ",
    "to `FALSE` to change this.\n"
  )
  
  invisible()
}
# nocov end
