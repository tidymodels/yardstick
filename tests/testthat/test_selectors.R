context("Tidy Selectors")

library(testthat)
library(yardstick)
library(tidyselect)

###################################################################

test_that('both', {
  expect_equal(
    yardstick:::all_select(two_class_example,
                           truth = truth,
                           estimate = predicted,
                           Class1),
    list(truth = "truth", estimate = "predicted", probs = "Class1")
  )
  expect_equal(
    yardstick:::all_select(two_class_example, truth, predicted, Class1, Class2),
    list(truth = "truth", estimate = "predicted", probs = c("Class1", "Class2"))
  )
  expect_equal(
    yardstick:::all_select(two_class_example,
                           truth = truth,
                           estimate = predicted,
                           matches("Class")),
    list(truth = "truth", estimate = "predicted", probs = c("Class1", "Class2"))
  )
  expect_error(
    yardstick:::all_select(two_class_example,
                           truth = truth,
                           estimate = matches("Class"))
  )
  expect_error(
    yardstick:::all_select(two_class_example, truth = tru)
  )
  expect_error(
    yardstick:::all_select(two_class_example,
                           truth = tru,
                           estimate = predicted,
                           Class2)
  )
  expect_error(
    yardstick:::all_select(two_class_example,
                           truth = truth,
                           estimate = predicted,
                           Class)
  )
})
