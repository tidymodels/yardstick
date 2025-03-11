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

test_that("validate_ordered_truth_matrix_estimate errors as expected for binary", {
  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
      1:3,
      estimator = "binary"
    )
  )

  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(c("a"), levels = c("a", "b")),
      1,
      estimator = "binary"
    )
  )

  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(character(), levels = c("a", "b")),
      numeric(),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      c("a", "b", "a"),
      1:3,
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
      c("a", "b", "a"),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(character(), levels = c("a", "b")),
      matrix(1:6, ncol = 2),
      estimator = "binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b", "c")),
      1:3,
      estimator = "binary"
    )
  )
})

test_that("validate_ordered_truth_matrix_estimate errors as expected for non-binary", {
  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
      matrix(1:6, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b", "c", "d")),
      matrix(1:12, ncol = 4),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(c("a"), levels = c("a", "b")),
      matrix(1:2, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_no_error(
    validate_ordered_truth_matrix_estimate(
      ordered(character(), levels = c("a", "b")),
      matrix(numeric(), ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      c("a", "b", "a"),
      matrix(1:6, ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
      1:3,
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
      matrix(as.character(1:6), ncol = 2),
      estimator = "non binary"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_ordered_truth_matrix_estimate(
      ordered(c("a", "b", "a"), levels = c("a", "b")),
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

  expect_no_error(
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj,
      lung_surv$.pred_time
    )
  )

  expect_no_error(
    validate_surv_truth_numeric_estimate(
      survival::Surv(1, 0),
      lung_surv$.pred_time[1]
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate("1", 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj,
      as.character(lung_surv$.pred_time)
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_numeric_estimate(
      lung_surv$surv_obj[1:5, ],
      lung_surv$.pred_time
    )
  )
})

test_that("validate_surv_truth_list_estimate errors as expected", {
  lung_surv <- data_lung_surv()
  lung_surv$list <- lapply(seq_len(nrow(lung_surv)), identity)
  lung_surv$list2 <- lapply(
    seq_len(nrow(lung_surv)),
    function(x) data.frame(wrong = 1, names = 2)
  )
  lung_surv$list3 <- lapply(
    lung_surv$.pred,
    function(x) x[c(1, 2, 5)]
  )
  lung_surv$list4 <- lapply(
    lung_surv$.pred,
    function(x) x[c(1, 2, 3)]
  )

  expect_no_error(
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      lung_surv$.pred
    )
  )

  expect_no_error(
    validate_surv_truth_list_estimate(
      survival::Surv(1, 0),
      lung_surv$.pred[1]
    )
  )

  expect_no_error(
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      lung_surv$list3
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate("1", 1)
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      lung_surv$list
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      lung_surv$list2
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      lung_surv$list4
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj,
      as.character(lung_surv$.pred_time)
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj[1:5, ],
      lung_surv$.pred_time
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv$surv_obj[1:5, ],
      lung_surv$.pred
    )
  )

  lung_surv_not_all_same <- lung_surv
  lung_surv_not_all_same$.pred[[5]]$.eval_time[1] <- 350
  lung_surv_not_all_same$.pred[[10]]$.eval_time[1] <- 350
  lung_surv_not_all_same$.pred[[14]]$.eval_time[1] <- 350
  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv_not_all_same$surv_obj,
      lung_surv_not_all_same$.pred
    )
  )

  lung_surv_neg <- lung_surv[1, ]
  lung_surv_neg$.pred[[1]]$.eval_time[1] <- -100
  rep()
  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv_neg$surv_obj,
      lung_surv_neg$.pred
    )
  )

  lung_surv_na <- lung_surv[1, ]
  lung_surv_na$.pred[[1]]$.eval_time[1] <- NA
  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv_na$surv_obj,
      lung_surv_na$.pred
    )
  )

  lung_surv_inf <- lung_surv[1, ]
  lung_surv_inf$.pred[[1]]$.eval_time[1] <- Inf
  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv_inf$surv_obj,
      lung_surv_inf$.pred
    )
  )

  lung_surv_duplicate <- lung_surv[1, ]
  lung_surv_duplicate$.pred[[1]]$.eval_time[1] <- 200
  expect_snapshot(
    error = TRUE,
    validate_surv_truth_list_estimate(
      lung_surv_duplicate$surv_obj,
      lung_surv_duplicate$.pred
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
