metric_tweak <- function(name, fn, fn_vec, ...) {
  if (!rlang::is_string(name)) {
    abort("`name` must be a string.")
  }
  if (!is_metric(fn)) {
    abort("`fn` must be a metric function.")
  }
  if (!rlang::is_function(fn_vec)) {
    abort("`fn_vec` must be a function.")
  }

  fixed <- rlang::enquos(...)

  if (length(fixed) > 0 && !rlang::is_named(fixed)) {
    abort("All optional arguments passed through `...` must be named.")
  }

  if (is_numeric_metric(fn)) {
    metric_tweak_numeric(name, fn, fn_vec, fixed)
  } else if (is_class_metric(fn)) {
    metric_tweak_class(name, fn, fn_vec, fixed)
  } else if (is_prob_metric(fn)) {
    metric_tweak_prob(name, fn, fn_vec, fixed)
  } else {
    abort("Unsupported metric function type.")
  }
}

# ------------------------------------------------------------------------------

metric_tweak_numeric <- function(name, fn, fn_vec, fixed) {
  check_protected_name(fixed, protected_names_numeric())

  new_fn_vec <- function(truth,
                         estimate,
                         na_rm = TRUE,
                         ...) {
    args <- list(
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      ...
    )

    rlang::exec(fn_vec, !!!args, !!!fixed)
  }

  new_fn <- function(data,
                     truth,
                     estimate,
                     na_rm = TRUE,
                     ...) {
    metric_summarizer(
      metric_nm = name,
      metric_fn = new_fn_vec,
      data = data,
      truth = {{truth}},
      estimate = {{estimate}},
      na_rm = na_rm,
      ... = ...
    )
  }

  direction <- attr(fn, "direction", exact = TRUE)
  new_fn <- new_numeric_metric(new_fn, direction)

  new_fn
}

metric_tweak_class <- function(name, fn, fn_vec, fixed) {
  check_protected_name(fixed, protected_names_class())

  new_fn_vec <- function(truth,
                         estimate,
                         estimator = NULL,
                         na_rm = TRUE,
                         event_level = yardstick_event_level(),
                         ...) {
    args <- list(
      truth = truth,
      estimate = estimate,
      estimator = estimator,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )

    rlang::exec(fn_vec, !!!args, !!!fixed)
  }

  new_fn <- function(data,
                     truth,
                     estimate,
                     estimator = NULL,
                     na_rm = TRUE,
                     event_level = yardstick_event_level(),
                     ...) {
    metric_summarizer(
      metric_nm = name,
      metric_fn = new_fn_vec,
      data = data,
      truth = {{truth}},
      estimate = {{estimate}},
      estimator = estimator,
      na_rm = na_rm,
      event_level = event_level,
      ... = ...
    )
  }

  direction <- attr(fn, "direction", exact = TRUE)
  new_fn <- new_class_metric(new_fn, direction)

  new_fn
}

metric_tweak_prob <- function(name, fn, fn_vec, fixed) {
  check_protected_name(fixed, protected_names_prob())

  new_fn_vec <- function(truth,
                         estimate,
                         estimator = NULL,
                         na_rm = TRUE,
                         event_level = yardstick_event_level(),
                         ...) {
    args <- list(
      truth = truth,
      estimate = estimate,
      estimator = estimator,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )

    rlang::exec(fn_vec, !!!args, !!!fixed)
  }

  new_fn <- function(data,
                     truth,
                     ...,
                     estimator = NULL,
                     na_rm = TRUE,
                     event_level = yardstick_event_level()) {
    estimate <- dots_to_estimate(data, !!! enquos(...))

    metric_summarizer(
      metric_nm = name,
      metric_fn = new_fn_vec,
      data = data,
      truth = {{truth}},
      estimate = {{estimate}},
      estimator = estimator,
      na_rm = na_rm,
      event_level = event_level
    )
  }

  direction <- attr(fn, "direction", exact = TRUE)
  new_fn <- new_prob_metric(new_fn, direction)

  new_fn
}

# ------------------------------------------------------------------------------

check_protected_name <- function(fixed, protected) {
  if (!has_protected_name(fixed, protected)) {
    return(invisible(fixed))
  }

  protected <- quote_and_collapse(protected)

  abort(paste0(
    "Optional arguments passed through `...` cannot be named any of: ",
    protected,
    "."
  ))
}

has_protected_name <- function(fixed, protected) {
  any(names(fixed) %in% protected)
}

protected_names_numeric <- function() {
  c("data", "truth", "estimate", "na_rm")
}
protected_names_class <- function() {
  c("data", "truth", "estimate", "estimator", "na_rm", "event_level")
}
protected_names_prob <- function() {
  c("data", "truth", "estimator", "na_rm", "event_level")
}
