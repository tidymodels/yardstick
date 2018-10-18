get_weights <- function(data, averaging) {

  if (averaging == "binary") {

    NULL

  }
  else if (averaging == "macro") {

    n <- nrow(data)
    rep(1 / n, times = n)

  }
  else if (averaging == "micro") {

    1

  }
  else if (averaging == "macro_weighted") {

    .col_sums <- colSums(data)
    .col_sums / sum(.col_sums)

  }
  else {

    msg <- paste0("Averaging type `", averaging, "` is unknown.")
    rlang::abort(msg)

  }
}

# ------------------------------------------------------------------------------

finalize_averaging <- function(x, averaging) {
  if (!is.null(averaging)) return(averaging)
  UseMethod("finalize_averaging")
}

finalize_averaging.default <- function(x, averaging) {
  "binary"
}

finalize_averaging.matrix <- function(x, averaging) {
  "macro"
}

finalize_averaging.numeric <- function(x, averaging) {
  "binary"
}

finalize_averaging.table <- function(x, averaging) {
  n_col <- ncol(x)

  # binary
  if (n_col == 2) {
    return("binary")
  }

  # multiclass
  if (n_col > 2) {
    return("macro")
  }
}

finalize_averaging.factor <- function(x, averaging) {
  lvls <- levels(x)

  # binary
  if (length(lvls) == 2) {
    return("binary")
  }

  # multiclass
  if (length(lvls) > 2) {
    return("macro")
  }

}
