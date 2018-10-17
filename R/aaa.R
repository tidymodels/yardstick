#' @useDynLib yardstick, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

## Taken from https://github.com/tidyverse/dplyr/blob/d310ad1cef1c14d770c94e1a9a4c79c888f46af6/R/zzz.r#L2-L9

# nocov start
# tested abyway in test_two_class.R
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.yardstick <- list(
    yardstick.event_first = TRUE
  )
  toset <- !(names(op.yardstick) %in% names(op))
  if (any(toset)) options(op.yardstick[toset])

  s3_register("ggplot2::autoplot", "gain_df")
  s3_register("ggplot2::autoplot", "lift_df")

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

# vctrs/register-s3.R
# https://github.com/r-lib/vctrs/blob/master/R/register-s3.R
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(method)) {
    method <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(method))

  if (package %in% loadedNamespaces()) {
    registerS3method(generic, class, method, envir = asNamespace(package))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      registerS3method(generic, class, method, envir = asNamespace(package))
    }
  )
}

# nocov end
