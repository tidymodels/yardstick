test_that("Calculations are correct - two class", {
  df <- data_altman()$pathology

  expect_identical(detection_rate_vec(df$pathology, df$scan), 231 / 344)
  expect_identical(
    detection_rate_vec(df$pathology, df$scan, event_level = "second"),
    54 / 344
  )
})

test_that("Calculations are correct - three class", {
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

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- unclass(path_tbl)

  exp <- detection_rate_vec(pathology$pathology, pathology$scan)

  expect_identical(
    detection_rate(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    detection_rate(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    detection_rate(pathology, truth = pathology, estimate = scan)[[
      ".estimate"
    ]],
    exp
  )
})

test_that("Grouped data and metric sets", {
  df <- tibble::tibble(
    group = c(1, 1, 2, 2),
    truth = factor(c("a", "b", "a", "b")),
    estimate = factor(c("a", "b", "b", "a"))
  )

  out <- detection_rate(dplyr::group_by(df, group), truth, estimate)

  expect_identical(out$group, c(1, 2))
  expect_identical(out$.estimate, c(1 / 2, 0))
  expect_identical(out$.metric, rep("detection_rate", 2))
  expect_identical(out$.estimator, rep("binary", 2))

  metrics <- metric_set(detection_rate, accuracy)
  expect_identical(
    metrics(df, truth, estimate = estimate)$.estimate,
    c(1 / 4, 1 / 2)
  )
  expect_identical(
    attr(attr(get_metrics("class"), "metrics")$detection_rate, "direction"),
    "maximize"
  )
  expect_identical(metric_range(detection_rate), c(0, 1))
})

test_that("Case weights calculations are correct", {
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

    expect_identical(
      detection_rate(df, truth, estimate, case_weights = weight)$.estimate,
      1 / 10
    )
    expect_identical(
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

test_that("Case weights calculations are correct - three class", {
  df <- tibble::tibble(
    truth = factor(c("a", "b", "b", "c", "c", "c")),
    estimate = factor(c("a", "a", "b", "b", "c", "c")),
    weight = hardhat::frequency_weights(1:6)
  )

  expect_identical(
    detection_rate(df, truth, estimate, case_weights = weight)$.estimate,
    15 / 63
  )
  expect_identical(
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

test_that("Calculations handles NAs", {
  df <- tibble::tibble(
    truth = factor(c("a", "a", NA, "b", "b", "a")),
    estimate = factor(c("a", NA, "a", "b", "a", "a")),
    weight = c(2, 3, 4, 5, 6, NA)
  )

  expect_identical(
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
  expect_identical(
    detection_rate_vec(altman$pathology, altman$scan_na),
    230 / 341
  )
})

test_that("Unused levels contribute zero", {
  truth <- factor(c("a", "a"), levels = c("a", "b", "c"))
  estimate <- truth

  expect_identical(detection_rate_vec(truth, estimate), 1 / 3)
  expect_identical(
    detection_rate_vec(truth, estimate, estimator = "micro"),
    1 / 3
  )
  expect_identical(
    detection_rate_vec(truth, estimate, estimator = "macro_weighted"),
    1
  )

  truth <- factor(c("b", "b"), levels = c("a", "b"))
  expect_identical(detection_rate_vec(truth, truth), 0)
  expect_identical(detection_rate_vec(truth, truth, event_level = "second"), 1)
})

test_that("Empty input and zero weights return missing values", {
  for (levels in list(c("a", "b"), c("a", "b", "c"))) {
    empty <- factor(character(), levels = levels)
    truth <- factor("a", levels = levels)

    expect_identical(detection_rate_vec(empty, empty), NA_real_)
    expect_true(is.na(detection_rate_vec(truth, truth, case_weights = 0)))
    expect_true(is.na(detection_rate_vec(truth, truth[NA_integer_])))
  }
})

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  truth <- factor(c("a", "a", "b", "b"))
  estimate <- factor(c("a", "b", "b", "a"))
  class_estimate <- probably::as_class_pred(estimate, which = 2)

  expect_identical(detection_rate_vec(truth, class_estimate), 1 / 3)
  expect_identical(
    detection_rate_vec(truth, class_estimate, na_rm = FALSE),
    NA_real_
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(probably::as_class_pred(truth, which = 1), estimate)
  )
})

test_that("bad argument check", {
  truth <- factor(c("a", "b"))
  estimate <- truth

  expect_snapshot(
    error = TRUE,
    detection_rate_vec(truth, estimate, na_rm = "yes")
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(as.character(truth), estimate)
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(truth, factor(c("a", "c"), levels = c("a", "c")))
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(truth, estimate, case_weights = 1)
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(truth, estimate, estimator = "unknown")
  )
  expect_snapshot(
    error = TRUE,
    detection_rate_vec(truth, estimate, event_level = "unknown")
  )
  expect_snapshot(error = TRUE, detection_rate(matrix(1, nrow = 2, ncol = 3)))
})

test_that("range values are correct", {
  range <- metric_range(detection_rate)

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  for (estimator in c("macro", "micro", "macro_weighted")) {
    perfect <- detection_rate_vec(df$truth, df$truth, estimator = estimator)
    worst <- detection_rate_vec(df$truth, df$off, estimator = estimator)

    expect_gte(perfect, range[1])
    expect_lte(perfect, range[2])
    expect_gte(worst, range[1])
    expect_lte(worst, range[2])
  }

  expect_identical(detection_rate_vec(df$truth, df$truth), 2 / 5)
  expect_identical(detection_rate_vec(df$truth, df$off), 0)
})
