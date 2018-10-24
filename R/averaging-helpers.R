get_weights <- function(data, averaging) {

  if (averaging == "binary") {

    NULL

  }
  else if (averaging == "macro") {

    n <- ncol(data)
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

#' @rdname developer-helpers
#'
#' @param x An object to dispatch the auto-selection off of. This is generally
#' the `truth` column.
#'
#' @param averaging Either `NULL` for auto-selection, or a single character
#' for the type of averaging to use.
#'
#' @seealso [metric_summarizer()] [metric_vec_template()]
#'
#' @export
finalize_averaging <- function(x, averaging) {
  if (!is.null(averaging)) return(averaging)
  UseMethod("finalize_averaging")
}

finalize_averaging.default <- function(x, averaging) {
  cls <- class(x)[[1]]
  abort(paste0(
    "Averaging cannot be automatically determined from object of class `",
    cls,
    "`."
  ))
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
