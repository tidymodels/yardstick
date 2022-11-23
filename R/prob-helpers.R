# AUC helper -------------------------------------------------------------------

# AUC by trapezoidal rule:
# https://en.wikipedia.org/wiki/Trapezoidal_rule
# assumes x is a partition and that x & y are the same length
auc <- function(x, y, na_rm = TRUE) {
  if(na_rm) {
    comp <- stats::complete.cases(x, y)
    x <- x[comp]
    y <- y[comp]
  }

  if (is.unsorted(x, na.rm = TRUE, strictly = FALSE)) {
    abort("`x` must already be in weakly increasing order.", .internal = TRUE)
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

# `...` -> estimate matrix / vector helper -------------------------------------

#' Developer helpers
#'
#' Helpers to be used alongside [check_metric], [handle_missings] and
#' [metric summarizers][class_metric_summarizer()] when creating new metrics. See [Custom
#' performance metrics](https://www.tidymodels.org/learn/develop/metrics/) for
#' more information.
#'
#' @section Dots -> Estimate:
#'
#' `dots_to_estimate()` is useful with class probability metrics that take
#' `...` rather than `estimate` as an argument. It constructs either a single
#' name if 1 input is provided to `...` or it constructs a quosure where the
#' expression constructs a matrix of as many columns as are provided to `...`.
#' These are eventually evaluated in the `summarise()` call in
#' [metric-summarizers()] and evaluate to either a vector or a matrix for
#' further use in the underlying vector functions.
#'
#'
#' @name developer-helpers
#'
#' @aliases dots_to_estimate
#'
#' @export
#'
#' @inheritParams roc_auc
dots_to_estimate <- function(data, ...) {

  # Capture dots
  dot_vars <- rlang::with_handlers(
    tidyselect::vars_select(names(data), !!! enquos(...)),
    tidyselect_empty_dots = function(cnd) {
      abort("No valid variables provided to `...`.")
    }
  )

  # estimate is a matrix of the selected columns if >1 selected
  dot_nms <- lapply(dot_vars, as.name)

  if (length(dot_nms) > 1) {
    estimate <- quo(
      matrix(
        data = c(!!! dot_nms),
        ncol = !!length(dot_nms),
        dimnames = list(NULL, !!dot_vars)
      )
    )
  }
  else {
    estimate <- dot_nms[[1]]
  }


  estimate
}

# One vs all helper ------------------------------------------------------------

one_vs_all_impl <- function(fn,
                            truth,
                            estimate,
                            case_weights,
                            ...) {
  lvls <- levels(truth)
  other <- "..other"

  metric_lst <- rlang::new_list(n = length(lvls))

  # one vs all
  for(i in seq_along(lvls)) {

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

one_vs_all_with_level <- function(fn,
                                  truth,
                                  estimate,
                                  case_weights,
                                  ...) {

  res <- one_vs_all_impl(
    fn = fn,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights,
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
