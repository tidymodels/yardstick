test_that("Binary detection rate is TP/N", {
  df <- data_altman()$pathology

  expect_equal(detection_rate_vec(df$pathology, df$scan), 231 / 344)
  expect_equal(
    detection_rate_vec(df$pathology, df$scan, event_level = "second"),
    54 / 344
  )
})

test_that("Multiclass averaging", {
  tbl <- data_three_by_three()

  expect_equal(detection_rate(tbl)[[".estimate"]], 12 / 60)
  expect_equal(
    detection_rate(tbl, estimator = "macro")[[".estimate"]],
    12 / 60
  )
  expect_equal(
    detection_rate(tbl, estimator = "micro")[[".estimate"]],
    12 / 60
  )
  expect_equal(
    detection_rate(tbl, estimator = "macro_weighted")[[".estimate"]],
    84 / 400
  )
  expect_equal(
    detection_rate(tbl)[[".estimate"]],
    accuracy(tbl)[[".estimate"]] / ncol(tbl)
  )
})

test_that("Interfaces agree", {
  lst <- data_altman()
  df <- lst$pathology
  tbl <- lst$path_tbl
  mat <- unclass(tbl)
  expected <- tibble::tibble(
    .metric = "detection_rate",
    .estimator = "binary",
    .estimate = 231 / 344
  )

  expect_equal(detection_rate(df, pathology, scan), expected)
  expect_equal(detection_rate(tbl), expected)
  expect_equal(detection_rate(mat), expected)
  expect_equal(
    detection_rate(mat, event_level = "second")[[".estimate"]],
    54 / 344
  )
})

test_that("Grouped data and metric sets", {
  df <- tibble::tibble(
    group = c(1, 1, 2, 2),
    truth = factor(c("a", "b", "a", "b")),
    estimate = factor(c("a", "b", "b", "a"))
  )

  out <- detection_rate(dplyr::group_by(df, group), truth, estimate)

  expect_equal(out$group, c(1, 2))
  expect_equal(out$.estimate, c(1 / 2, 0))
  expect_equal(out$.metric, rep("detection_rate", 2))
  expect_equal(out$.estimator, rep("binary", 2))

  metrics <- metric_set(detection_rate, accuracy)
  expect_equal(
    metrics(df, truth, estimate = estimate)$.estimate,
    c(1 / 4, 1 / 2)
  )
  expect_equal(
    attr(attr(get_metrics("class"), "metrics")$detection_rate, "direction"),
    "maximize"
  )
  expect_equal(metric_range(detection_rate), c(0, 1))
})

test_that("Case weights", {
  df <- tibble::tibble(
    truth = factor(c("a", "a", "b", "b")),
    estimate = factor(c("a", "b", "a", "b")),
    weight = c(1, 2, 3, 4)
  )

  for (weights in list(
    df$weight,
    hardhat::frequency_weights(df$weight),
    hardhat::importance_weights(df$weight / 10)
  )) {
    df$weight <- weights

    expect_equal(
      detection_rate(df, truth, estimate, case_weights = weight)$.estimate,
      1 / 10
    )
    expect_equal(
      detection_rate_vec(
        df$truth,
        df$estimate,
        case_weights = weights,
        event_level = "second"
      ),
      4 / 10
    )
  }
})

test_that("Multiclass case weights", {
  df <- tibble::tibble(
    truth = factor(c("a", "b", "b", "c", "c", "c")),
    estimate = factor(c("a", "a", "b", "b", "c", "c")),
    weight = hardhat::frequency_weights(1:6)
  )

  expect_equal(
    detection_rate(df, truth, estimate, case_weights = weight)$.estimate,
    15 / 63
  )
  expect_equal(
    detection_rate(
      df,
      truth,
      estimate,
      case_weights = weight,
      estimator = "micro"
    )$.estimate,
    15 / 63
  )
  expect_equal(
    detection_rate(
      df,
      truth,
      estimate,
      case_weights = weight,
      estimator = "macro_weighted"
    )$.estimate,
    181 / 441
  )
})

test_that("Missing truth, estimate and weights", {
  df <- tibble::tibble(
    truth = factor(c("a", "a", NA, "b", "b", "a")),
    estimate = factor(c("a", NA, "a", "b", "a", "a")),
    weight = c(2, 3, 4, 5, 6, NA)
  )

  expect_equal(
    detection_rate(df, truth, estimate, case_weights = weight)$.estimate,
    2 / 13
  )

  for (column in c("truth", "estimate", "weight")) {
    complete <- df[c(1, 4, 5), ]
    complete[[column]][1] <- NA

    expect_identical(
      detection_rate_vec(
        complete$truth,
        complete$estimate,
        case_weights = complete$weight,
        na_rm = FALSE
      ),
      NA_real_
    )
  }

  altman <- data_altman()$pathology
  expect_equal(
    detection_rate_vec(altman$pathology, altman$scan_na),
    230 / 341
  )
})

test_that("Unused levels contribute zero", {
  truth <- factor(c("a", "a"), levels = c("a", "b", "c"))
  estimate <- truth

  expect_equal(detection_rate_vec(truth, estimate), 1 / 3)
  expect_equal(
    detection_rate_vec(truth, estimate, estimator = "micro"),
    1 / 3
  )
  expect_equal(
    detection_rate_vec(truth, estimate, estimator = "macro_weighted"),
    1
  )

  truth <- factor(c("b", "b"), levels = c("a", "b"))
  expect_equal(detection_rate_vec(truth, truth), 0)
  expect_equal(detection_rate_vec(truth, truth, event_level = "second"), 1)
})

test_that("Empty input and zero weights return missing values", {
  for (levels in list(c("a", "b"), c("a", "b", "c"))) {
    empty <- factor(character(), levels = levels)
    truth <- factor("a", levels = levels)

    expect_true(is.na(detection_rate_vec(empty, empty)))
    expect_true(is.na(detection_rate_vec(truth, truth, case_weights = 0)))
    expect_true(is.na(detection_rate_vec(truth, truth[NA_integer_])))
  }
})

test_that("Equivocal class predictions are missing", {
  skip_if_not_installed("probably")

  truth <- factor(c("a", "a", "b", "b"))
  estimate <- factor(c("a", "b", "b", "a"))
  class_estimate <- probably::as_class_pred(estimate, which = 2)

  expect_equal(detection_rate_vec(truth, class_estimate), 1 / 3)
  expect_identical(
    detection_rate_vec(truth, class_estimate, na_rm = FALSE),
    NA_real_
  )
  expect_error(
    detection_rate_vec(probably::as_class_pred(truth, which = 1), estimate)
  )
})

test_that("Invalid inputs are rejected", {
  truth <- factor(c("a", "b"))
  estimate <- truth

  expect_error(detection_rate_vec(truth, estimate, na_rm = "yes"))
  expect_error(detection_rate_vec(as.character(truth), estimate))
  expect_error(
    detection_rate_vec(truth, factor(c("a", "c"), levels = c("a", "c")))
  )
  expect_error(detection_rate_vec(truth, estimate, case_weights = 1))
  expect_error(detection_rate_vec(truth, estimate, estimator = "unknown"))
  expect_error(detection_rate_vec(truth, estimate, event_level = "unknown"))
  expect_error(detection_rate(matrix(1, nrow = 2, ncol = 3)))
})
