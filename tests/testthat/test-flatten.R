test_that("flat tables", {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

  expect_identical(
    unname(yardstick:::flatten(three_class_tb)),
    as.vector(three_class_tb)
  )
  expect_equal(
    names(yardstick:::flatten(three_class_tb[1:2, 1:2])),
    c("cell_1_1", "cell_2_1", "cell_1_2", "cell_2_2")
  )

  expect_snapshot(
    error = TRUE,
    yardstick:::flatten(three_class_tb[, 1:2])
  )
})
