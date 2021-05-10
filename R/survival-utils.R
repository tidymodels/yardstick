# ------------------------------------------------------------------------------

# For use with `cls` in `metric_vec_template()`
is.Surv <- function(x) {
  inherits(x, "Surv")
}

# ------------------------------------------------------------------------------

# Check that each element of `estimate` has the appropriate structure,
# i.e., each element is a data frame with `.time` and `.pred_survival` columns,
# both of which are numeric.
validate_surv_estimate <- function(estimate) {
  lapply(estimate, validate_surv_estimate_one)
  invisible(estimate)
}
validate_surv_estimate_one <- function(x) {
  if (!is.data.frame(x)) {
    abort("Each element of `estimate` must be a data frame.")
  }

  if (ncol(x) != 2L) {
    abort("Each element of `estimate` must have two columns.")
  }

  if (!all(names(x) %in% c(".time", ".pred_survival"))) {
    abort("Each element of `estimate` must have column names of '.time' and '.pred_survival'.")
  }

  time <- x[[".time"]]
  pred_survival <- x[[".pred_survival"]]

  if (!is.integer(time) && !is.double(time)) {
    abort("All elements of `estimate$.time` must be numeric.")
  }

  if (!is.integer(pred_survival) && !is.double(pred_survival)) {
    abort("All elements of `estimate$.pred_survival` must be numeric.")
  }

  if (anyNA(time)) {
    abort("All elements of `estimate$.time` must not be missing.")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

convert_surv_to_tibble <- function(x) {
  # TODO: What about non-right censored?
  type <- attr(x, "type", exact = TRUE)

  if (!identical(type, "right")) {
    abort("Survival metrics in yardstick currently only support right censoring.")
  }

  x <- unclass(x)

  if (ncol(x) != 2L) {
    abort("Right censored Surv objects should be a matrix with two columns.")
  }

  time <- x[, 1, drop = TRUE]
  status <- x[, 2, drop = TRUE]

  # Ensure they are jointly NA
  missing <- is.na(time) | is.na(status)

  if (any(missing)) {
    time[missing] <- NA
    status[missing] <- NA
  }

  dplyr::tibble(time = time, status = status)
}

# ------------------------------------------------------------------------------

prepare_naive_surv_tbl <- function(truth, estimate) {
  # Additional structural checks on `estimate`
  validate_surv_estimate(estimate)

  df <- convert_surv_to_tibble(truth)
  df$estimate <- estimate

  df <- tidyr::unnest(df, estimate)

  # `indicator` treats the "event" as death,
  # so we should supply the probability of death, not survival
  df <- dplyr::mutate(df, pred_event = 1L - .pred_survival)
  df <- dplyr::select(df, -.pred_survival)

  # Remove all subjects censored before `.time`.
  # Removes the effect of censoring for the "naive" estimate.
  # Propagate `NA` values to handle them later.
  remove <- (df$time < df$.time) & (df$status == 0L)
  keep <- (!remove) | is.na(df$time) | is.na(df$.time)
  df <- vctrs::vec_slice(df, keep)

  # If `time` occurred before the cutoff `.time`, we have an event
  df <- dplyr::mutate(
    df,
    indicator = dplyr::if_else(
      condition = time <= .time,
      true = "event",
      false = "non-event",
      missing = NA_character_
    ),
    indicator = factor(indicator, levels = c("event", "non-event"))
  )

  df
}

# ------------------------------------------------------------------------------

utils::globalVariables(c(
  ".estimate",
  "indicator",
  "pred_event",
  ".pred_survival",
  "time",
  ".time"
))
