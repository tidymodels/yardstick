test_that("validate_numeric_truth_numeric_estimate errors as expected", {
  expect_no_error(
    validate_numeric_truth_numeric_estimate(1:10, 1:10)
  )

  expect_no_error(
    validate_numeric_truth_numeric_estimate(1, 1)
  )

  expect_no_error(
    validate_numeric_truth_numeric_estimate(1L, 1L)
  )

  expect_no_error(
    validate_numeric_truth_numeric_estimate(numeric(), numeric())
  )

  expect_snapshot(
    error = TRUE,
    validate_numeric_truth_numeric_estimate("1", 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_numeric_truth_numeric_estimate(1, "1")
  )

  expect_snapshot(
    error = TRUE,
    validate_numeric_truth_numeric_estimate(matrix(1), 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_numeric_truth_numeric_estimate(1, matrix(1))
  )

  expect_snapshot(
    error = TRUE,
    validate_numeric_truth_numeric_estimate(1:4, 1:5)
  )
})

test_that("validate_factor_truth_factor_estimate errors as expected", {
  expect_no_error(
    validate_factor_truth_factor_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      factor(c("a", "a", "a"), levels = c("a", "b"))
    )
  )

  expect_no_error(
    validate_factor_truth_factor_estimate(
      factor(c("a"), levels = c("a")),
      factor(c("a"), levels = c("a"))
    )
  )

  expect_no_error(
    validate_factor_truth_factor_estimate(
      factor(character(), levels = character()),
      factor(character(), levels = character())
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate("1", 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate(
      c("a", "b", "a"),
      factor(c("a", "a", "a"), levels = c("a", "b"))
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate(
      factor(c("a", "a", "a"), levels = c("a", "b")),
      c("a", "b", "a")
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate(
      factor(c("a", "a", "a"), levels = c("a", "b")),
      1:3
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      factor(c("a", "a", "a"), levels = c("a", "b", "c"))
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_factor_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      factor(c("a", "a", "a", "a"), levels = c("a", "b"))
    )
  )
})

test_that("validate_factor_truth_matrix_estimate errors as expected for binary", {
  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      1:3,
      estimator = "binary"
    )
  )

  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(c("a"), levels = c("a", "b")),
      1,
      estimator = "binary"
    )
  )

  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(character(), levels = c("a", "b")),
      numeric(),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      c("a", "b", "a"),
      1:3,
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      c("a", "b", "a"),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(character(), levels = c("a", "b")),
      matrix(1:6, ncol = 2),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      1:3,
      estimator = "binary"
    )
  )
})

test_that("validate_factor_truth_matrix_estimate errors as expected for non-binary", {
  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      matrix(1:6, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b", "c", "d")),
      matrix(1:12, ncol = 4),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(c("a"), levels = c("a", "b")),
      matrix(1:2, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_factor_truth_matrix_estimate(
      factor(character(), levels = c("a", "b")),
      matrix(numeric(), ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      c("a", "b", "a"),
      matrix(1:6, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      1:3,
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      matrix(as.character(1:6), ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_factor_truth_matrix_estimate(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      matrix(1:15, ncol = 5),
      estimator = "non binary"
    )
  )
})


test_that("validate_numeric_truth_numeric_estimate errors as expected", {
  expect_no_error(
    validate_binary_estimator(
      factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimator = "not binary"
    )
  )

  expect_no_error(
    validate_binary_estimator(
      factor(c("a", "b", "a"), levels = c("a", "b")),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_binary_estimator(
      factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimator = "binary"
    )
  )
})

test_that("validate_surv_truth_numeric_estimate errors as expected", {
  lung_surv <- data_lung_surv()
  lung_surv$list <- lapply(seq_len(nrow(lung_surv)), identity)
  lung_surv$list2 <- lapply(
    seq_len(nrow(lung_surv)),
    function(x) data.frame(wrong = 1, names = 2)
  )

  expect_no_error(
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj,
      lung_surv$age
    )
  )

  expect_no_error(
    validate_surv_truth_numeric_estimate(
      survival::Surv(1, 0),
      lung_surv$age[1]
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate("1", 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$list,
      lung_surv$age
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$list2,
      lung_surv$age
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj,
      as.character(lung_surv$inst)
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj[1:5, ],
      lung_surv$age
    )
  )
})

test_that("validate_case_weights errors as expected", {
  expect_no_error(
    validate_case_weights(NULL, 10)
  )

  expect_no_error(
    validate_case_weights(1:10, 10)
  )

  expect_snapshot(
    error = TRUE,
    validate_case_weights(1:10, 11)
  )
})
