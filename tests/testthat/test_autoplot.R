library(testthat)
library(dplyr)
library(ggplot2)

context("Autoplot")

set.seed(123)

two_class_resamples <- bind_rows(
  replicate(10, sample_n(two_class_example, 300, TRUE), simplify = FALSE),
  .id = "Resample"
) %>%
  group_by(Resample)

# make it smaller, and order it in the same order as what ggplot2 displays
hpc_cv2 <- filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")) %>%
  as_tibble() %>%
  group_by(Resample) %>%
  arrange(as.character(obs)) %>%
  ungroup()

# ROC --------------------------------------------------------------------------

test_that("ROC Curve - two class", {
  res <- roc_curve(two_class_example, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  # x and y data
  expect_equal(1 - res$specificity, .plot_data$data[[1]]$x)
  expect_equal(res$sensitivity, .plot_data$data[[1]]$y)

  # 45 degree line
  expect_equal(.plot_data$data[[2]]$intercept, 0)
  expect_equal(.plot_data$data[[2]]$slope, 1)
})

test_that("ROC Curve - two class, with resamples", {
  res <- roc_curve(two_class_resamples, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  expect_equal(1 - res$specificity, .plot_data$data[[1]]$x)
  expect_equal(res$sensitivity, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)
})

test_that("ROC Curve - multi class", {
  res <- roc_curve(hpc_cv2, obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(nrow(.plot_data$data[[2]]), 4)
})

test_that("ROC Curve - multi class, with resamples", {
  res <- roc_curve(group_by(hpc_cv2, Resample), obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(nrow(.plot_data$data[[2]]), 4)
})

# PR ---------------------------------------------------------------------------

test_that("PR Curve - two class", {
  res <- pr_curve(two_class_example, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  # x and y data
  expect_equal(res$recall, .plot_data$data[[1]]$x)
  expect_equal(res$precision, .plot_data$data[[1]]$y)

})

test_that("PR Curve - two class, with resamples", {
  res <- pr_curve(two_class_resamples, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  expect_equal(res$recall, .plot_data$data[[1]]$x)
  expect_equal(res$precision, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)
})

test_that("PR Curve - multi class", {
  res <- pr_curve(hpc_cv2, obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)
})

test_that("PR Curve - multi class, with resamples", {
  res <- pr_curve(group_by(hpc_cv2, Resample), obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 5)
})

# Gain -------------------------------------------------------------------------

test_that("Gain Curve - two class", {
  res <- gain_curve(two_class_example, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  # x and y data
  expect_equal(res$.percent_tested, .plot_data$data[[1]]$x)
  expect_equal(res$.percent_found, .plot_data$data[[1]]$y)

  # polygon "perfect" corner
  expect_equal(.plot_data$data[[2]]$x[2], 51.6)

})

test_that("Gain Curve - two class, with resamples", {
  res <- gain_curve(two_class_resamples, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  expect_equal(res$.percent_tested, .plot_data$data[[1]]$x)
  expect_equal(res$.percent_found, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)

  # polygon "perfect" corner (min of the resamples)
  expect_equal(.plot_data$data[[2]]$x[2], 43 + 2/3)
})

test_that("Gain Curve - multi class", {
  res <- gain_curve(hpc_cv2, obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # polygon "perfect" corner (one per level)
  corners <- c(2, 5, 8, 11)
  corner_vals <- c(31.0623556581986, 5.94688221709007, 11.9515011547344, 51.0392609699769)
  expect_equal(.plot_data$data[[2]]$x[corners], corner_vals)
})

test_that("Gain Curve - multi class, with resamples", {
  res <- gain_curve(group_by(hpc_cv2, Resample), obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 5)

  # polygon "perfect" corner (one per level)
  corners <- c(2, 5, 8, 11)
  corner_vals <- c(30.9248554913295, 5.78034682080925, 11.8155619596542, 50.8620689655172)
  expect_equal(.plot_data$data[[2]]$x[corners], corner_vals)
})

# Lift -------------------------------------------------------------------------

test_that("Lift Curve - two class", {
  res <- lift_curve(two_class_example, truth, Class1)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  # first row has NA and is removed
  res <- res[-1,]

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

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  .plot_data <- ggplot_build(.plot)

  # 10 rows removed (the 0 event rows)
  expect_equal(nrow(.plot_data$data[[1]]), nrow(res) - 10)

  # 0 event rows are removed before plotting
  res <- filter(res, .n_events != 0)

  expect_equal(res$.percent_tested, .plot_data$data[[1]]$x)
  expect_equal(res$.lift, .plot_data$data[[1]]$y)

  # number of unique colors
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 10)

})

test_that("Lift Curve - multi class", {
  res <- lift_curve(hpc_cv2, obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

})

test_that("Lift Curve - multi class, with resamples", {
  res <- lift_curve(group_by(hpc_cv2, Resample), obs, VF:L)

  expect_error(.plot <- autoplot(res), NA)
  expect_is(.plot, "gg")

  expect_true(".level" %in% colnames(res))
  expect_true("Resample" %in% colnames(res))

  .plot_data <- ggplot_build(.plot)

  # 4 panels
  expect_equal(length(unique(.plot_data$data[[1]]$PANEL)), 4)

  # 5 resamples
  expect_equal(length(unique(.plot_data$data[[1]]$colour)), 5)
})
