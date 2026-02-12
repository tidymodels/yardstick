test_that("Calculations are correct", {
  pROC_two_class_example_curve <- data_pROC_two_class_example_curve()

  # Equal to pROC up to a reasonable tolerance
  expect_equal(
    roc_curve(two_class_example, truth, Class1),
    pROC_two_class_example_curve
  )

  # HPC_CV takes too long
  hpc_cv2 <- dplyr::filter(
    hpc_cv,
    Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")
  )

  res <- roc_curve(hpc_cv2, obs, VF:L)

  # structural tests
  expect_equal(
    colnames(res),
    c(".level", ".threshold", "specificity", "sensitivity")
  )
  expect_equal(unique(res$.level), levels(hpc_cv2$obs))

  res_g <- roc_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  # structural tests
  expect_equal(
    colnames(res_g),
    c("Resample", ".level", ".threshold", "specificity", "sensitivity")
  )
})

test_that("na_rm = FALSE errors if missing values are present", {
  df <- two_class_example
  df$Class1[1] <- NA

  expect_snapshot(
    error = TRUE,
    roc_curve_vec(df$truth, df$Class1, na_rm = FALSE)
  )
})

test_that("Case weights calculations are correct", {
  #grouped multiclass (one-vs-all) weighted example matches expanded equivalent
  hpc_cv$weight <- rep(1, times = nrow(hpc_cv))
  hpc_cv$weight[c(100, 200, 150, 2)] <- 5

  hpc_cv <- dplyr::group_by(hpc_cv, Resample)

  hpc_cv_expanded <- hpc_cv[
    vec_rep_each(seq_len(nrow(hpc_cv)), times = hpc_cv$weight),
  ]

  expect_identical(
    roc_curve(hpc_cv, obs, VF:L, case_weights = weight),
    roc_curve(hpc_cv_expanded, obs, VF:L)
  )

  two_class_example$weight <- read_weights_two_class_example()

  curve <- roc_curve(two_class_example, truth, Class1, case_weights = weight)

  expect_identical(
    curve,
    read_pydata("py-roc-curve")$case_weight$binary
  )

  # If they weren't removed, we'd get a `NaN` from a division by zero issue
  df <- dplyr::tibble(
    truth = factor(c("b", "a", "b", "a", "a"), levels = c("a", "b")),
    a = c(0.75, 0.7, 0.4, 0.9, 0.8),
    weight = c(0, 1, 3, 0, 5)
  )

  expect_identical(
    roc_curve(df, truth, a, case_weights = weight),
    roc_curve(df[df$weight != 0, ], truth, a, case_weights = weight)
  )
})

test_that("works with hardhat case weights", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(0.9, 0.8, 0.7, 0.68, 0.5),
    weight = c(2, 1, 1, 3, 2)
  )

  curve1 <- roc_curve(df, truth, estimate, case_weights = weight)

  df$weight <- hardhat::frequency_weights(df$weight)

  curve2 <- roc_curve(df, truth, estimate, case_weights = weight)

  expect_identical(curve1, curve2)
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- two_class_example$Class1

  expect_snapshot(
    error = TRUE,
    roc_curve_vec(cp_truth, estimate)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    roc_curve_vec(1, 1, na_rm = "yes")
  )
})

test_that("binary roc curve uses equivalent of pROC `direction = <`", {
  # In yardstick we do events (or cases) as the first level
  truth <- factor(c("control", "case", "case"), levels = c("case", "control"))

  # Make really bad predictions
  # This would force `direction = "auto"` to choose `>`,
  # which would be incorrect. We are required to force `direction = <` for
  # our purposes of having `estimate` match the event
  estimate <- c(0.8, 0.2, 0.1)

  # # pROC expects levels to be in the order of control, then event.
  # roc <- pROC::roc(
  #   truth,
  #   estimate,
  #   levels = c("control", "case"),
  #   direction = "<"
  # )
  # expect_specificity <- c(0, roc$specificities)
  # expect_sensitivity <- c(1, roc$sensitivities)
  expect_specificity <- c(0, 0, 0, 0, 1)
  expect_sensitivity <- c(1, 1, 0.5, 0, 0)

  curve <- roc_curve_vec(truth, estimate)

  expect_identical(curve$specificity, expect_specificity)
  expect_identical(curve$sensitivity, expect_sensitivity)
})

test_that("sklearn equivalent", {
  curve <- roc_curve(two_class_example, truth, Class1)

  expect_identical(
    curve,
    read_pydata("py-roc-curve")$binary
  )
})

test_that("roc_curve() - error is thrown when missing events", {
  no_event <- dplyr::filter(two_class_example, truth == "Class2")

  expect_snapshot(
    error = TRUE,
    roc_curve_vec(no_event$truth, no_event$Class1)[[".estimate"]]
  )
})

test_that("roc_curve() - error is thrown when missing controls", {
  no_control <- dplyr::filter(two_class_example, truth == "Class1")

  expect_snapshot(
    error = TRUE,
    roc_curve_vec(no_control$truth, no_control$Class1)[[".estimate"]]
  )
})

test_that("roc_curve() - multiclass one-vs-all approach results in error", {
  no_event <- dplyr::filter(hpc_cv, Resample == "Fold01", obs == "VF")

  expect_snapshot(
    error = TRUE,
    roc_curve_vec(no_event$obs, as.matrix(dplyr::select(no_event, VF:L)))[[
      ".estimate"
    ]]
  )
})

test_that("roc_curve() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_snapshot({
    out <- roc_curve(two_class_example, truth, Class1, options = 1)
  })

  expect_identical(
    out,
    roc_curve(two_class_example, truth, Class1)
  )
})

test_that("thresholds argument works", {
  two_class_example0 <- two_class_example
  two_class_example0$Class1 <- floor(two_class_example0$Class1 * 10) / 10
  exp <- roc_curve(two_class_example0, truth, Class1)

  res <- roc_curve(
    two_class_example,
    truth,
    Class1,
    thresholds = seq(0, 0.9, by = 0.1)
  )

  expect_equal(res, exp)

  reverse <- roc_curve(
    two_class_example,
    truth,
    Class1,
    thresholds = seq(0.9, 0, by = -0.1)
  )
  expect_equal(reverse, exp)

  duplicated <- roc_curve(
    two_class_example,
    truth,
    Class1,
    thresholds = c(seq(0, 0.9, by = 0.1), seq(0, 0.9, by = 0.1))
  )
  expect_equal(duplicated, exp)
})

test_that("thresholds argument throws errors when wrongly specied", {
  expect_snapshot(
    error = TRUE,
    roc_curve(
      two_class_example,
      truth,
      Class1,
      thresholds = TRUE
    )
  )

  expect_snapshot(
    error = TRUE,
    roc_curve(
      two_class_example,
      truth,
      Class1,
      thresholds = -4
    )
  )

  expect_snapshot(
    error = TRUE,
    roc_curve(
      two_class_example,
      truth,
      Class1,
      thresholds = seq(-1, 2, by = 0.2)
    )
  )
})
