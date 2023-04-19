test_that("Matches MLmetrics", {
  x <- c(1, 1.2, 1.6, 2)
  y <- c(4, 3.8, 4.2, 5)
  # MLmetrics::Area_Under_Curve(x, y, "trapezoid")
  auc_known <- 4.22

  expect_equal(auc(x, y), auc_known)
})
