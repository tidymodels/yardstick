metric_tweak <- function(.name, .fn, ...) {
  if (!rlang::is_string(.name)) {
    rlang::abort("`.name` must be a string.")
  }
  if (!is_metric(.fn)) {
    rlang::abort("`.fn` must be a metric function.")
  }

  fixed <- rlang::enquos(...)

  if (length(fixed) > 0 && !rlang::is_named(fixed)) {
    rlang::abort("All arguments passed through `...` must be named.")
  }

  check_protected_names(fixed)

  out <- function(...) {
    args <- rlang::enquos(...)
    call <- rlang::call2(.fn, !!!args, !!!fixed)
    out <- rlang::eval_tidy(call)
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

  rlang::abort(paste0(
    "Arguments passed through `...` cannot be named any of: ",
    protected,
    "."
  ))
}
protected_names <- function() {
  c("data", "truth", "estimate")
}
