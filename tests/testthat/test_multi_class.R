library(testthat)
library(yardstick)
library(purrr)

context("Multi-class metrics")

# This is just a small data set with easily calculatable results

multi_ex <- as.table(
  matrix(
    c(3, 1, 1,
      0, 4, 2,
      1, 3, 5
    ),
    ncol = 3, byrow = TRUE,
    dimnames = list(c("c1", "c2", "c3"), c("c1", "c2", "c3"))
  )
)

weighted_macro_weights <- colSums(multi_ex) / sum(colSums(multi_ex))

# turn a 3x3 conf mat into a 2x2 submatrix in a one vs all approach
make_submat <- function(data, col) {
  top_left <- data[col, col]
  top_righ <- sum(data[col,-col])
  bot_left <- sum(data[-col,col])
  bot_righ <- sum(data[-col,-col])
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
macro_metric <- function(binary_metric, ...) {
  mean(vapply(multi_submats, binary_metric, numeric(1), ...))
}

macro_weighted_metric <- function(binary_metric, ...) {
  weighted.mean(vapply(multi_submats, binary_metric, numeric(1), ...), weighted_macro_weights)
}

# for micro, we calculate the sum of the numerator and
# divide by the sum of the denom to get the micro metric
micro <- function(.numer, .denom) {
  .numer <- rlang::as_function(.numer)
  .denom <- rlang::as_function(.denom)

  .numer <- sum(vapply(multi_submats, .numer, double(1)))
  .denom <- sum(vapply(multi_submats, .denom, double(1)))

  .numer / .denom
}

###################################################################

test_that('sensitivity', {

  # sens = recall
  expect_equal(
    sens(multi_ex, averaging = "macro")[[".estimate"]],
    macro_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, averaging = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, averaging = "micro")[[".estimate"]],
    micro(.numer = ~.x[1,1], .denom = ~sum(.x[,1]))
  )
})


test_that('specificity', {

  expect_equal(
    spec(multi_ex, averaging = "macro")[[".estimate"]],
    macro_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, averaging = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, averaging = "micro")[[".estimate"]],
    micro(.numer = ~.x[2,2], .denom = ~sum(.x[,2]))
  )
})

test_that('recall', {

  # sens = recall
  expect_equal(
    recall(multi_ex, averaging = "macro")[[".estimate"]],
    macro_metric(recall_binary)
  )
  expect_equal(
    recall(multi_ex, averaging = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(recall_binary)
  )
  expect_equal(
    recall(multi_ex, averaging = "micro")[[".estimate"]],
    micro(.numer = ~.x[1,1], .denom = ~sum(.x[,1]))
  )
})

test_that('precision', {

  # sens = recall
  expect_equal(
    precision(multi_ex, averaging = "macro")[[".estimate"]],
    macro_metric(precision_binary)
  )
  expect_equal(
    precision(multi_ex, averaging = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(precision_binary)
  )
  expect_equal(
    precision(multi_ex, averaging = "micro")[[".estimate"]],
    micro(.numer = ~.x[1,1], .denom = ~sum(.x[1,]))
  )
})

test_that('f_meas', {

  # sens = recall
  expect_equal(
    f_meas(multi_ex, averaging = "macro")[[".estimate"]],
    macro_metric(f_meas_binary)
  )
  expect_equal(
    f_meas(multi_ex, averaging = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(f_meas_binary)
  )

  # https://turi.com/products/create/docs/generated/graphlab.evaluation.fbeta_score.html
  turi_ex <- tibble(
    tru = as.factor(c(0, 1, 2, 3, 0, 1, 2, 3)),
    pre = as.factor(c(1, 0, 2, 1, 3, 1, 0, 1))
  )

  expect_equal(
    f_meas(turi_ex, tru, pre, averaging = "micro")[[".estimate"]],
    .25
  )
})
