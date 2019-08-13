try_cor <- function(truth, estimate) {
  handler <- make_cor_handler(truth, estimate)

  withCallingHandlers(
    expr = cor(truth, estimate),
    simpleWarning = handler
  )
}

make_cor_handler <- function(truth, estimate) {
  handle_zero_variance <- function(cnd) {
    if (cnd$message != "the standard deviation is zero") {
      return(invisible())
    }

    n_unique_truth <- length(unique(truth))
    n_unique_estimate <- length(unique(estimate))

    if (n_unique_truth == 1L) {
      warn_correlation_undefined_constant_truth(truth)
      rlang::cnd_muffle(cnd)
    }

    if (n_unique_estimate == 1L) {
      warn_correlation_undefined_constant_estimate(estimate)
      rlang::cnd_muffle(cnd)
    }

    invisible()
  }

  handle_zero_variance
}

warn_correlation_undefined_constant_truth <- function(truth) {
  warn_correlation_undefined(
    what = "truth",
    truth = truth,
    .subclass = "yardstick_warning_correlation_undefined_constant_truth"
  )
}

warn_correlation_undefined_constant_estimate <- function(estimate) {
  warn_correlation_undefined(
    what = "estimate",
    estimate = estimate,
    .subclass = "yardstick_warning_correlation_undefined_constant_estimate"
  )
}

warn_correlation_undefined <- function(what, ..., .subclass = character()) {
  message <- paste0(
    "A correlation computation is required, but `", what, "` is constant ",
    "and has 0 standard deviation, resulting in a divide by 0 error. ",
    "`NA` will be returned."
  )

  rlang::warn(
    message = message,
    .subclass = c(.subclass, "yardstick_warning_correlation_undefined"),
    ...
  )
}

