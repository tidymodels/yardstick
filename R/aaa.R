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

  # Dynamic registration of tidy.conf_mat() so we don't need to
  # import broom
  if (requireNamespace("broom", quietly = TRUE)) {
    register_s3_method("broom", "tidy", "conf_mat")
  }

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

# function is called in .onLoad()
# adapted from
# https://github.com/tidyverse/googledrive/blob/master/R/dplyr-compat.R
register_s3_method <- function(pkg, generic, class, fun = NULL) {

  is_string <- function(x) length(x) == 1L && is.character(x)

  stopifnot(is_string(pkg))
  envir <- asNamespace(pkg)

  stopifnot(is_string(generic))
  stopifnot(is_string(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}

# nocov end
