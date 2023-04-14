#' Tweak a metric function
#'
#' @description
#' `metric_tweak()` allows you to tweak an existing metric `.fn`, giving it a
#' new `.name` and setting new optional argument defaults through `...`. It
#' is similar to `purrr::partial()`, but is designed specifically for yardstick
#' metrics.
#'
#' `metric_tweak()` is especially useful when constructing a [metric_set()] for
#' tuning with the tune package. After the metric set has been constructed,
#' there is no way to adjust the value of any optional arguments (such as
#' `beta` in [f_meas()]). Using `metric_tweak()`, you can set optional arguments
#' to custom values ahead of time, before they go into the metric set.
#'
#' @details
#' The function returned from `metric_tweak()` only takes `...` as arguments,
#' which are passed through to the original `.fn`. Passing `data`, `truth`,
#' and `estimate` through by position should generally be safe, but it is
#' recommended to pass any other optional arguments through by name to ensure
#' that they are evaluated correctly.
#'
#' @param .name A single string giving the name of the new metric. This will be
#'   used in the `".metric"` column of the output.
#'
#' @param .fn An existing yardstick metric function to tweak.
#'
#' @param ... Name-value pairs specifying which optional arguments to override
#'   and the values to replace them with.
#'
#'   Arguments `data`, `truth`, and `estimate` are considered _protected_,
#'   and cannot be overridden, but all other optional arguments can be
#'   altered.
#'
#' @return
#' A tweaked version of `.fn`, updated to use new defaults supplied in `...`.
#'
#' @export
#' @examples
#' mase12 <- metric_tweak("mase12", mase, m = 12)
#'
#' # Defaults to `m = 1`
#' mase(solubility_test, solubility, prediction)
#'
#' # Updated to use `m = 12`. `mase12()` has this set already.
#' mase(solubility_test, solubility, prediction, m = 12)
#' mase12(solubility_test, solubility, prediction)
#'
#' # This is most useful to set optional argument values ahead of time when
#' # using a metric set
#' mase10 <- metric_tweak("mase10", mase, m = 10)
#' metrics <- metric_set(mase, mase10, mase12)
#' metrics(solubility_test, solubility, prediction)
metric_tweak <- function(.name, .fn, ...) {
  if (!is_string(.name)) {
    abort("`.name` must be a string.")
  }
  if (!is_metric(.fn)) {
    abort("`.fn` must be a metric function.")
  }

  fixed <- enquos(...)

  if (length(fixed) > 0 && !is_named(fixed)) {
    abort("All arguments passed through `...` must be named.")
  }

  check_protected_names(fixed)

  out <- function(...) {
    args <- enquos(...)
    call <- call2(.fn, !!!args, !!!fixed)
    out <- eval_tidy(call)
    out[[".metric"]] <- .name
    out
  }

  class(out) <- class(.fn)
  metric_direction(out) <- metric_direction(.fn)

  out
}

# ------------------------------------------------------------------------------

check_protected_names <- function(fixed) {
  protected <- protected_names()
  has_protected_name <- any(names(fixed) %in% protected)

  if (!has_protected_name) {
    return(invisible(fixed))
  }

  protected <- quote_and_collapse(protected)

  abort(paste0(
    "Arguments passed through `...` cannot be named any of: ",
    protected,
    "."
  ))
}
protected_names <- function() {
  c("data", "truth", "estimate")
}
