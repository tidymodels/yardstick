test_that("binary - uses user defined costs", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    2,
    "B",
    "A",
    3
  )

  exp_cost <-
    1.00 * 3 + 0.00 * 0 + 0.80 * 0 + 0.20 * 2 + 0.51 * 3 + 0.49 * 0

  expect_equal(
    classification_cost(df, obs, A, costs = costs)[[".estimate"]],
    exp_cost / nrow(df)
  )
})

test_that("binary - respects `event_first`", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    B = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    2,
    "B",
    "A",
    3
  )

  exp_cost <-
    1.00 * 0 + 0.00 * 3 + 0.80 * 2 + 0.20 * 0 + 0.51 * 0 + 0.49 * 3

  expect_equal(
    classification_cost(df, obs, B, costs = costs, event_level = "second")[[
      ".estimate"
    ]],
    exp_cost / nrow(df)
  )
})

test_that("costs$truth can be factor", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    2,
    "B",
    "A",
    3
  )

  costs2 <- costs
  costs2$truth <- as.factor(costs2$truth)
  costs2$estimate <- as.factor(costs2$estimate)

  expect_identical(
    classification_cost(df, obs, A, costs = costs),
    classification_cost(df, obs, A, costs = costs2)
  )
})

test_that("binary - requires 1 column of probabilities", {
  expect_snapshot(
    error = TRUE,
    classification_cost(two_class_example, truth, Class1:Class2)
  )
})

# ------------------------------------------------------------------------------

test_that("multiclass - uses equal costs by default", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4)
  )

  A <- df$B[df$obs == "A"] + df$C[df$obs == "A"]
  B <- df$A[df$obs == "B"] + df$C[df$obs == "B"]
  C <- df$A[df$obs == "C"] + df$B[df$obs == "C"]

  estimate <- mean(c(A, B, C))

  expect_equal(
    classification_cost(df, obs, A:C)[[".estimate"]],
    estimate
  )
})

test_that("multiclass - respects user defined costs", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4)
  )
  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "A",
    0,
    "A",
    "B",
    1,
    "A",
    "C",
    2,
    "B",
    "A",
    3,
    "B",
    "B",
    0,
    "B",
    "C",
    4,
    "C",
    "A",
    5,
    "C",
    "B",
    6,
    "C",
    "C",
    0,
  )

  exp_cost <-
    1.00 *
    0 +
    0.00 * 1 +
    0.00 * 2 +
    0.80 * 0 +
    0.05 * 1 +
    0.15 * 2 +
    0.51 * 0 +
    0.29 * 1 +
    0.20 * 2 +
    0.10 * 3 +
    0.80 * 0 +
    0.10 * 4 +
    0.20 * 3 +
    0.60 * 0 +
    0.20 * 4 +
    0.30 * 5 +
    0.30 * 6 +
    0.40 * 0

  expect_equal(
    classification_cost(df, obs, A:C, costs = costs)[[".estimate"]],
    exp_cost / nrow(df)
  )
})

test_that("multiclass - fills in missing combinations with zero cost", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4)
  )
  costs_partial <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "A",
    0,
    # "A",    "B",          1,
    "A",
    "C",
    2,
    "B",
    "A",
    3,
    # "B",    "B",          0,
    "B",
    "C",
    4,
    "C",
    "A",
    5,
    # "C",    "B",          6,
    "C",
    "C",
    0,
  )
  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "A",
    0,
    "A",
    "B",
    0,
    "A",
    "C",
    2,
    "B",
    "A",
    3,
    "B",
    "B",
    0,
    "B",
    "C",
    4,
    "C",
    "A",
    5,
    "C",
    "B",
    0,
    "C",
    "C",
    0,
  )

  expect_identical(
    classification_cost(df, obs, A:C, costs = costs_partial),
    classification_cost(df, obs, A:C, costs = costs)
  )
})

# ------------------------------------------------------------------------------

test_that("costs must be a data frame with the right column names", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = 1)
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = data.frame())
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = data.frame(x = 1, y = 2, z = 3))
  )
})

test_that("costs$estimate must contain the right levels", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "b",
    2,
    "B",
    "A",
    3
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )
})

test_that("costs$truth must contain the right levels", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "a",
    "B",
    2,
    "B",
    "A",
    3
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )
})

test_that("costs$truth, costs$estimate, and costs$cost must have the right type", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    1,
    "B",
    2,
    2,
    "A",
    3
  )
  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    1,
    2,
    "B",
    2,
    3
  )
  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    "1",
    "B",
    "A",
    "2"
  )
  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )
})

test_that("costs$truth and costs$estimate cannot contain duplicate pairs", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    2,
    "A",
    "B",
    3
  )

  expect_snapshot(
    error = TRUE,
    classification_cost(df, obs, A, costs = costs)
  )
})

# ------------------------------------------------------------------------------

test_that("binary - uses case weights", {
  df <- data.frame(
    obs = factor(c("B", "A", "B"), levels = c("A", "B")),
    A = c(1, .80, .51),
    weight = c(1, 2, 2)
  )

  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "B",
    2,
    "B",
    "A",
    3
  )

  exp_cost <-
    ((1.00 * 3 + 0.00 * 0) * 1L) +
    ((0.80 * 0 + 0.20 * 2) * 2L) +
    ((0.51 * 3 + 0.49 * 0) * 2L)

  exp_cost <- exp_cost / sum(df$weight)

  expect_equal(
    classification_cost(df, obs, A, costs = costs, case_weights = weight)[[
      ".estimate"
    ]],
    exp_cost
  )
})

test_that("multiclass - uses case weights", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4),
    weight = c(1, 2, 3, 4, 5, 6)
  )
  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "A",
    "A",
    0,
    "A",
    "B",
    1,
    "A",
    "C",
    2,
    "B",
    "A",
    3,
    "B",
    "B",
    0,
    "B",
    "C",
    4,
    "C",
    "A",
    5,
    "C",
    "B",
    6,
    "C",
    "C",
    0,
  )

  exp_cost <-
    ((1.00 * 0 + 0.00 * 1 + 0.00 * 2) * 1) +
    ((0.80 * 0 + 0.05 * 1 + 0.15 * 2) * 2) +
    ((0.51 * 0 + 0.29 * 1 + 0.20 * 2) * 3) +
    ((0.10 * 3 + 0.80 * 0 + 0.10 * 4) * 4) +
    ((0.20 * 3 + 0.60 * 0 + 0.20 * 4) * 5) +
    ((0.30 * 5 + 0.30 * 6 + 0.40 * 0) * 6)

  exp_cost <- exp_cost / sum(df$weight)

  expect_equal(
    classification_cost(df, obs, A:C, costs = costs, case_weights = weight)[[
      ".estimate"
    ]],
    exp_cost
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    classification_cost_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    classification_cost_vec(df$truth, df$Class1, case_weights = freq_wgt)
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- two_class_example$Class1

  expect_snapshot(
    error = TRUE,
    classification_cost_vec(cp_truth, estimate)
  )
})
