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
