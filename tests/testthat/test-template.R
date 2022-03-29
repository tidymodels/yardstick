test_that("missing values in case weights are considered by `na_rm`", {
  truth <- factor(c("x", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "x"), levels = c("x", "y"))
  case_weights <- c(1, NA)

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights),
    1
  )

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights, na_rm = FALSE),
    NA_real_
  )
})

test_that("case weights are validated", {
  truth <- factor(c("x", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "x"), levels = c("x", "y"))

  expect_snapshot(error = TRUE, {
    accuracy_vec(truth, estimate, case_weights = 1)
  })
  expect_snapshot(error = TRUE, {
    accuracy_vec(truth, estimate, case_weights = c("x", "y"))
  })
})
