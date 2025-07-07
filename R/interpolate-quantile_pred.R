# Move these to hardhat? --------------------------------------------------

#' restrict various objects to the interval \[lower, upper\]
#' @param x the object to restrict
#' @param lower numeric, the lower bound
#' @param upper numeric, the upper bound
#' @param ... unused
#' @export
#' @keywords internal
snap <- function(x, lower, upper, ...) {
  UseMethod("snap")
}

#' @export
snap.default <- function(x, lower, upper, ...) {
  rlang::check_dots_empty()
  check_number_decimal(lower)
  check_number_decimal(upper)
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} should be should be numeric,
      not {.obj_type_friendly {x}}."
    )
  }
  pmin(pmax(x, lower), upper)
}

#' @export
snap.quantile_pred <- function(x, lower, upper, ...) {
  values <- as.matrix(x)
  quantile_levels <- x %@% "quantile_levels"
  values <- lapply(vctrs::vec_chop(values), function(.x) snap(.x, lower, upper))
  hardhat::quantile_pred(do.call(rbind, values), quantile_levels = quantile_levels)
}


impute_quantiles <- function(
    x, probs = seq(0, 1, 0.25), na.rm = FALSE, lower = -Inf,
    upper = Inf, middle = c("cubic", "linear"), ...)
{
  check_bool(na.rm)
  if (is.unsorted(probs)) probs <- sort(probs)
  hardhat::check_quantile_levels(probs)
  check_number_decimal(lower, allow_na = na.rm)
  check_number_decimal(upper, allow_na = na.rm)
  if (lower > upper) {
    cli::cli_abort("`lower` must be less than `upper`.")
  }
  middle <- rlang::arg_match(middle)
  snap(impute_quantile_internal(x, probs, middle), lower, upper)
}

impute_quantile_internal <- function(x, tau_out, middle) {
  tau <- x %@% "quantile_levels"
  qvals <- as.matrix(x)
  if (all(tau_out %in% tau) && !anyNA(qvals)) {
    return(qvals[, match(tau_out, tau), drop = FALSE])
  }
  if (length(tau) < 2) {
    cli::cli_abort(
      "Quantile interpolation is not possible when fewer than 2 quantiles
      are avaliable."
    )
  }
  qvals_out <- lapply(
    vctrs::vec_chop(qvals),
    function(.x) impute_quantiles_single(.x, tau, tau_out, middle)
  )
  qvals_out <- do.call(rbind, qvals_out)
  qvals_out
}

impute_quantiles_single <- function(qvals, tau, tau_out, middle) {
  qvals_out <- rep(NA, length(tau_out))
  good <- !is.na(qvals)
  if (!any(good)) {
    return(qvals_out)
  }
  qvals <- qvals[good]
  tau <- tau[good]

  # in case we only have one point, and it matches something we wanted
  if (length(good) < 2) {
    matched_one <- tau_out %in% tau
    qvals_out[matched_one] <- qvals[matched_one]
    return(qvals_out)
  }

  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr

  if (middle == "cubic") {
    method <- "cubic"
    result <- tryCatch(
      {
        Q <- stats::splinefun(tau, qvals, method = "hyman")
        quartiles <- Q(c(.25, .5, .75))
      },
      error = function(e) {
        return(NA)
      }
    )
  }
  if (middle == "linear" || any(is.na(result))) {
    method <- "linear"
    quartiles <- stats::approx(tau, qvals, c(.25, .5, .75))$y
  }
  if (any(indm)) {
    qvals_out[indm] <- switch(
      method,
      linear = stats::approx(tau, qvals, tau_out[indm])$y,
      cubic = Q(tau_out[indm])
    )
  }
  if (any(indl) || any(indr)) {
    qv <- data.frame(
      q = c(tau, tau_out[indm]),
      v = c(qvals, qvals_out[indm])
    ) %>%
      dplyr::distinct(q, .keep_all = TRUE) %>%
      dplyr::arrange(q)
  }
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(tau_out[indl], utils::head(qv, 2))
  }
  if (any(indr)) {
    qvals_out[indr] <- tail_extrapolate(tau_out[indr], utils::tail(qv, 2))
  }
  qvals_out
}

logit <- function(p) {
  p <- pmax(pmin(p, 1), 0)
  log(p) - log(1 - p)
}

# extrapolates linearly on the logistic scale using
# the two points nearest the tail
tail_extrapolate <- function(tau_out, qv) {
  if (nrow(qv) == 1L) {
    return(rep(qv$v[1], length(tau_out)))
  }
  x <- logit(qv$q)
  x0 <- logit(tau_out)
  y <- qv$v
  m <- diff(y) / diff(x)
  m * (x0 - x[1]) + y[1]
}
