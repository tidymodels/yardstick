context("classification cost")

# ------------------------------------------------------------------------------

ll_dat <- data.frame(
  obs  = factor(c("A", "A", "A", "B", "B", "C")),
  A = c(1, .80, .51, .1, .2, .3),
  B = c(0, .05, .29, .8, .6, .3),
  C = c(0, .15, .20, .1, .2, .4)
)

costs <- dplyr::tribble(
  ~truth, ~.pred_class, ~cost,
  "A",    "A",          0,
  "A",    "B",          1,
  "A",    "C",          2,
  "B",    "A",          3,
  "B",    "B",          0,
  "B",    "C",          4,
  "C",    "A",          5,
  "C",    "B",          6,
  "C",    "C",          0,
)

# ------------------------------------------------------------------------------

test_that('Three class', {

  exp_cost <-
    1.00 * 0 + 0.00 * 1 + 0.00 * 2 +
    0.80 * 0 + 0.05 * 1 + 0.15 * 2 +
    0.51 * 0 + 0.29 * 1 + 0.20 * 2 +
    0.10 * 3 + 0.80 * 0 + 0.10 * 4 +
    0.20 * 3 + 0.60 * 0 + 0.20 * 4 +
    0.30 * 5 + 0.30 * 6 + 0.40 * 0

  expect_equal(
    class_cost(ll_dat, obs, A:C, costs = costs)[[".estimate"]],
    exp_cost/nrow(ll_dat)
  )

  exp_cost_equal <-
    1.00 * 0 + 0.00 * 1 + 0.00 * 1 +
    0.80 * 0 + 0.05 * 1 + 0.15 * 1 +
    0.51 * 0 + 0.29 * 1 + 0.20 * 1 +
    0.10 * 1 + 0.80 * 0 + 0.10 * 1 +
    0.20 * 1 + 0.60 * 0 + 0.20 * 1 +
    0.30 * 1 + 0.30 * 1 + 0.40 * 0

  expect_equal(
    class_cost(ll_dat, obs, A:C)[[".estimate"]],
    exp_cost_equal/nrow(ll_dat)
  )

  exp_cost_incomplete <-
    1.00 * 0 + 0.00 * 1 + 0.00 * 2 +
    0.80 * 0 + 0.05 * 1 + 0.15 * 2 +
    0.51 * 0 + 0.29 * 1 + 0.20 * 2 +
    0.10 * 3 + 0.80 * 0 + 0.10 * 4 +
    0.20 * 3 + 0.60 * 0 + 0.20 * 4 +
    0.30 * 0 + 0.30 * 0 + 0.40 * 0

  expect_equal(
    class_cost(ll_dat, obs, A:C, costs = costs[1:6,])[[".estimate"]],
    exp_cost_incomplete/nrow(ll_dat)
  )


})


# ------------------------------------------------------------------------------

test_that('bad inputs', {
  bad_1 <-
    dplyr::mutate(costs, truth = dplyr::recode(truth, A = "a", B = "b", C = "c"))
  expect_error(class_cost(ll_dat, obs, A:C, costs = bad_1))

  bad_2 <-
    dplyr::mutate(costs, .pred_class = dplyr::recode(truth, A = "a", B = "b", C = "c"))
  expect_error(class_cost(ll_dat, obs, A:C, costs = bad_2))

  bad_3 <-
    dplyr::mutate(costs, cost = letters[1:9])
  expect_error(class_cost(ll_dat, obs, A:C, costs = bad_3))

  expect_error(class_cost(ll_dat, obs, A:C, costs = setNames(costs, letters[1:3])))
})

