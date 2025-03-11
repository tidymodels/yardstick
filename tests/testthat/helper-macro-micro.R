# These helpers are used to test macro and macro weighted methods

data_three_by_three <- function() {
  as.table(
    matrix(
      c(
        3,
        1,
        1,
        0,
        4,
        2,
        1,
        3,
        5
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(c("c1", "c2", "c3"), c("c1", "c2", "c3"))
    )
  )
}

multi_ex <- data_three_by_three()
weighted_macro_weights <- colSums(multi_ex) / sum(colSums(multi_ex))

# turn a 3x3 conf mat into a 2x2 submatrix in a one vs all approach
make_submat <- function(data, col) {
  top_left <- data[col, col]
  top_righ <- sum(data[col, -col])
  bot_left <- sum(data[-col, col])
  bot_righ <- sum(data[-col, -col])
  as.table(
    matrix(
      c(top_left, top_righ, bot_left, bot_righ),
      ncol = 2,
      byrow = TRUE
    )
  )
}

# These are the "one vs all" sub matrices
# for macro / weighted macro, calculate the binary version of each metric
# and then average them together
multi_submats <- list(
  c1 = make_submat(multi_ex, 1),
  c2 = make_submat(multi_ex, 2),
  c3 = make_submat(multi_ex, 3)
)

# Just pass in a binary metric function
macro_metric <- function(binary_metric, event_level = "first", ...) {
  mean(
    vapply(
      multi_submats,
      binary_metric,
      numeric(1),
      event_level = event_level,
      ...
    )
  )
}

macro_weighted_metric <- function(binary_metric, event_level = "first", ...) {
  stats::weighted.mean(
    vapply(
      multi_submats,
      binary_metric,
      numeric(1),
      event_level = event_level,
      ...
    ),
    weighted_macro_weights
  )
}

# For micro examples, we calculate the pieces by hand and use them individually
data_three_by_three_micro <- function() {
  res <- list(
    tp = vapply(
      multi_submats,
      function(x) {
        x[1, 1]
      },
      double(1)
    ),
    p = vapply(
      multi_submats,
      function(x) {
        colSums(x)[1]
      },
      double(1)
    ),
    tn = vapply(
      multi_submats,
      function(x) {
        x[2, 2]
      },
      double(1)
    ),
    n = vapply(
      multi_submats,
      function(x) {
        colSums(x)[2]
      },
      double(1)
    )
  )

  res <- c(
    res,
    list(
      fp = res$p - res$tp,
      fn = res$n - res$tn
    )
  )

  res
}
