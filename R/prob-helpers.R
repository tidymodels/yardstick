# AUC helper -------------------------------------------------------------------

# AUC by trapezoidal rule:
# https://en.wikipedia.org/wiki/Trapezoidal_rule
# assumes x is a partition and that x & y are the same length
auc <- function(x, y, na_rm = TRUE) {
  if (na_rm) {
    comp <- stats::complete.cases(x, y)
    x <- x[comp]
    y <- y[comp]
  }

  if (is.unsorted(x, na.rm = TRUE, strictly = FALSE)) {
    # should not be reachable
    cli::cli_abort(
      "{.arg x} must already be in weakly increasing order.",
      .internal = TRUE
    )
  }

  # length x = length y
  n <- length(x)

  # dx
  dx <- x[-1] - x[-n]

  # mid height of y
  height <- (y[-n] + y[-1]) / 2

  auc <- sum(height * dx)

  auc
}

# One vs all helper ------------------------------------------------------------

one_vs_all_impl <- function(fn, truth, estimate, case_weights, call, ...) {
  lvls <- levels(truth)
  other <- "..other"

  metric_lst <- new_list(n = length(lvls))

  # one vs all
  for (i in seq_along(lvls)) {
    # Recode truth into 2 levels, relevant and other
    # Pull out estimate prob column corresponding to relevant
    # Pulls by order, so they have to be in the same order as the levels!
    # (cannot pull by name because they arent always the same name i.e. .pred_{level})
    lvl <- lvls[i]

    truth_temp <- factor(
      x = ifelse(truth == lvl, lvl, other),
      levels = c(lvl, other)
    )

    estimate_temp <- as.numeric(estimate[, i])

    # `one_vs_all_impl()` always ignores the event level ordering when
    # computing each individual binary metric
    metric_lst[[i]] <- fn(
      truth_temp,
      estimate_temp,
      case_weights = case_weights,
      event_level = "first",
      ...
    )
  }

  metric_lst
}

one_vs_all_with_level <- function(
  fn,
  truth,
  estimate,
  case_weights,
  call,
  ...
) {
  res <- one_vs_all_impl(
    fn = fn,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights,
    call = call,
    ...
  )

  lvls <- levels(truth)

  with_level <- function(df, lvl) {
    df$.level <- lvl
    dplyr::select(df, .level, tidyselect::everything())
  }

  res <- mapply(
    with_level,
    df = res,
    lvl = lvls,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  dplyr::bind_rows(res)
}
