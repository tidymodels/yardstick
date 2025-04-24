skip_if_not_installed("ggplot2")

# As of R 3.6, cannot rely on old sample() results to be the same.
# Pre R 3.6, they were generated like this, and we have saved them
# as static values to be more reproducible

# set.seed(123)
# resample_idx <- replicate(
#   n = 10,
#   expr = sample.int(
#     n = nrow(two_class_example),
#     size = 300,
#     replace = TRUE
#   ),
#   simplify = FALSE
# )

# saveRDS(object = resample_idx, file = test_path("data/test_autoplot.rds"))
resample_idx <- readRDS(test_path("data/test_autoplot.rds"))

two_class_resamples <- dplyr::bind_rows(
  lapply(resample_idx, function(idx) two_class_example[idx, ]),
  .id = "Resample"
) |>
  dplyr::group_by(Resample)

# make it smaller, and order it in the same order as what ggplot2 displays
hpc_cv2 <- dplyr::filter(
  hpc_cv,
  Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")
) |>
  dplyr::as_tibble() |>
  dplyr::group_by(Resample) |>
  dplyr::arrange(as.character(obs)) |>
  dplyr::ungroup()

# ROC --------------------------------------------------------------------------

test_that("ROC Curve - two class", {
  res <- roc_curve(two_class_example, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # x and y data
  expect_equal(1 - res$specificity, .plot_data$data[[1]]$x)
  expect_equal(res$sensitivity, .plot_data$data[[1]]$y)

  # 45 degree line
  expect_equal(.plot_data$data[[2]]$intercept, 0)
  expect_equal(.plot_data$data[[2]]$slope, 1)
})

test_that("ROC Curve - two class, with resamples", {
  res <- roc_curve(two_class_resamples, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  expect_equal(1 - res$specificity, .plot_data$data[[1]]$x)
  expect_equal(res$sensitivity, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)
})

test_that("ROC Curve - multi class", {
  res <- roc_curve(hpc_cv2, obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(nrow(.plot_data$data[[2]]), 4)
})

test_that("ROC Curve - multi class, with resamples", {
  res <- roc_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(nrow(.plot_data$data[[2]]), 4)
})

# PR ---------------------------------------------------------------------------

test_that("PR Curve - two class", {
  res <- pr_curve(two_class_example, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # x and y data
  expect_equal(res$recall, .plot_data$data[[1]]$x)
  expect_equal(res$precision, .plot_data$data[[1]]$y)
})

test_that("PR Curve - two class, with resamples", {
  res <- pr_curve(two_class_resamples, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  expect_equal(res$recall, .plot_data$data[[1]]$x)
  expect_equal(res$precision, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)
})

test_that("PR Curve - multi class", {
  res <- pr_curve(hpc_cv2, obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)
})

test_that("PR Curve - multi class, with resamples", {
  res <- pr_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 5)
})

# Gain -------------------------------------------------------------------------

test_that("Gain Curve - two class", {
  res <- gain_curve(two_class_example, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # x and y data
  expect_equal(res$.percent_tested, .plot_data$data[[2]]$x)
  expect_equal(res$.percent_found, .plot_data$data[[2]]$y)

  # polygon "perfect" corner
  expect_equal(.plot_data$data[[1]]$x[2], 51.6)
})

test_that("Gain Curve - two class, with resamples", {
  res <- gain_curve(two_class_resamples, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  expect_equal(res$.percent_tested, .plot_data$data[[2]]$x)
  expect_equal(res$.percent_found, .plot_data$data[[2]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[2]]$colour)), 10)

  # polygon "perfect" corner (min of the resamples)
  expect_equal(.plot_data$data[[1]]$x[2], 43 + 2 / 3)
})

test_that("Gain Curve - multi class", {
  res <- gain_curve(hpc_cv2, obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[2]]$PANEL)), 4)

  # polygon "perfect" corner (one per level)
  corners <- c(2, 5, 8, 11)
  corner_vals <- c(
    31.0623556581986,
    5.94688221709007,
    11.9515011547344,
    51.0392609699769
  )
  expect_equal(.plot_data$data[[1]]$x[corners], corner_vals)
})

test_that("Gain Curve - multi class, with resamples", {
  res <- gain_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[2]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[2]]$colour)), 5)

  # polygon "perfect" corner (one per level)
  corners <- c(2, 5, 8, 11)
  corner_vals <- c(
    30.9248554913295,
    5.78034682080925,
    11.8155619596542,
    50.8620689655172
  )
  expect_equal(.plot_data$data[[1]]$x[corners], corner_vals)
})

# Lift -------------------------------------------------------------------------

test_that("Lift Curve - two class", {
  res <- lift_curve(two_class_example, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # first row has NA and is removed
  res <- res[-1, ]

  # 1 row removed
  expect_equal(nrow(.plot_data$data[[1]]), 500)

  # x and y data
  expect_equal(res$.percent_tested, .plot_data$data[[1]]$x)
  expect_equal(res$.lift, .plot_data$data[[1]]$y)

  # horizontal line
  expect_equal(.plot_data$data[[2]]$x, c(0, 100))
})

test_that("Lift Curve - two class, with resamples", {
  res <- lift_curve(two_class_resamples, truth, Class1)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 10 rows removed (the 0 event rows)
  expect_equal(nrow(.plot_data$data[[1]]), nrow(res) - 10)

  # 0 event rows are removed before plotting
  res <- dplyr::filter(res, .n_events != 0)

  expect_equal(res$.percent_tested, .plot_data$data[[1]]$x)
  expect_equal(res$.lift, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)
})

test_that("Lift Curve - multi class", {
  res <- lift_curve(hpc_cv2, obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)
})

test_that("Lift Curve - multi class, with resamples", {
  res <- lift_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  expect_no_error(.plot <- ggplot2::autoplot(res))
  expect_s3_class(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 5)
})

# Confusion Matrix  ------------------------------------------------------------
test_that("Confusion Matrix - type argument", {
  res <- conf_mat(two_class_example, truth, predicted)

  expect_snapshot(
    error = TRUE,
    ggplot2::autoplot(res, type = "wrong")
  )
})

test_that("Confusion Matrix - two class - heatmap", {
  res <- conf_mat(two_class_example, truth, predicted)

  expect_no_error(.plot <- ggplot2::autoplot(res, type = "heatmap"))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panes
  expect_equal(nrow(.plot_data$data[[1]]), length(res$table))
})

test_that("Confusion Matrix - multi class - heatmap", {
  res <- hpc_cv |>
    dplyr::filter(Resample == "Fold01") |>
    conf_mat(obs, pred)

  expect_no_error(.plot <- ggplot2::autoplot(res, type = "heatmap"))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # panes
  expect_equal(nrow(.plot_data$data[[1]]), length(res$table))

  # check dimensions
  expect_equal(rlang::expr_label(.plot$mapping[["x"]]), "`~Truth`")
  expect_equal(rlang::expr_label(.plot$mapping[["y"]]), "`~Prediction`")
  expect_equal(rlang::expr_label(.plot$mapping[["fill"]]), "`~Freq`")
})

test_that("Confusion Matrix - heatmap - can use non-standard labels (#157, #191)", {
  df <- dplyr::filter(hpc_cv, Resample == "Fold01")

  res1 <- conf_mat(df, obs, pred, dnn = c("Pred", "True"))
  expect_no_error(p1 <- ggplot2::autoplot(res1, type = "heatmap"))
  expect_identical(p1$labels$x, "True")
  expect_identical(p1$labels$y, "Pred")

  # Defaults are used when there are no names
  res2 <- conf_mat(df, obs, pred, dnn = NULL)
  expect_no_error(p2 <- ggplot2::autoplot(res2, type = "heatmap"))
  expect_identical(p2$labels$x, "Truth")
  expect_identical(p2$labels$y, "Prediction")
})

test_that("Confusion Matrix - mosaic - can use non-standard labels (#191)", {
  df <- dplyr::filter(hpc_cv, Resample == "Fold01")

  res1 <- conf_mat(df, obs, pred, dnn = c("Pred", "True"))
  expect_no_error(p1 <- ggplot2::autoplot(res1, type = "mosaic"))
  expect_identical(p1$labels$x, "True")
  expect_identical(p1$labels$y, "Pred")

  # Defaults are used when there are no names
  res2 <- conf_mat(df, obs, pred, dnn = NULL)
  expect_no_error(p2 <- ggplot2::autoplot(res2, type = "mosaic"))
  expect_identical(p2$labels$x, "Truth")
  expect_identical(p2$labels$y, "Prediction")
})

test_that("Confusion Matrix - two class - mosaic", {
  res <- conf_mat(two_class_example, truth, predicted)

  expect_no_error(.plot <- ggplot2::autoplot(res, type = "mosaic"))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # 4 panes
  expect_equal(nrow(.plot_data$data[[1]]), length(res$table))
})

test_that("Confusion Matrix - multi class - mosaic", {
  res <- hpc_cv |>
    dplyr::filter(Resample == "Fold01") |>
    conf_mat(obs, pred)

  expect_no_error(.plot <- ggplot2::autoplot(res, type = "mosaic"))
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # panes
  expect_equal(nrow(.plot_data$data[[1]]), length(res$table))
})
