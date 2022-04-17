# # For comparison against pROC in the `roc_curve()` tests
#
# curve <- pROC::roc(
#   two_class_example$truth,
#   two_class_example$Class1,
#   levels = rev(levels(two_class_example$truth)),
#   direction = "<"
# )
#
# points <- pROC::coords(
#   curve,
#   x = unique(c(-Inf, two_class_example$Class1, Inf)),
#   input = "threshold",
#   transpose = FALSE
# )
#
# points <- dplyr::as_tibble(points)
# points <- dplyr::arrange(points, threshold)
# points <- dplyr::rename(points, .threshold = threshold)
# class(points) <- c("roc_df", class(points))
#
# saveRDS(
#   object = points,
#   file = test_path("data", "helper-pROC-two-class-example-curve.rds"),
#   version = 2L
# )
data_pROC_two_class_example_curve <- function() {
  readRDS(test_path("data", "helper-pROC-two-class-example-curve.rds"))
}
