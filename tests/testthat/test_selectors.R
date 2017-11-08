library(testthat)
library(yardstick)
library(tidyselect)

###################################################################

test_that('just factors', {
  expect_equal(
    yardstick:::factor_select(two_class_example, truth = truth, estimate = predicted),
    list(truth = "truth", estimate = "predicted")
 )
  expect_equal(
    yardstick:::factor_select(two_class_example, truth, predicted),
    list(truth = "truth", estimate = "predicted")
  )  
  expect_equal(
    yardstick:::factor_select(two_class_example,
                              truth = truth,
                              estimate = predicted,
                              matches("Class")),
    list(truth = "truth", estimate = "predicted")
  )
  expect_error(
    yardstick:::factor_select(two_class_example,
                              truth = truth,
                              estimate = Class1,
                              matches("Class"))
  )
  expect_error(
    yardstick:::factor_select(two_class_example,
                              truth = truth,
                              estimate = matches("Class"))
  )
  expect_error(
    yardstick:::factor_select(two_class_example, truth = tru)
  )  
})

###################################################################

test_that('just probs', {
  expect_equal(
    yardstick:::prob_select(two_class_example, truth = truth, matches("^Class")),
    list(truth = "truth", probs = c("Class1", "Class2"))
  )
  expect_equal(
    yardstick:::prob_select(two_class_example, truth = truth, Class1),
    list(truth = "truth", probs = "Class1")
  )  
  expect_error(
    yardstick:::prob_select(two_class_example,
                            truth = truth,
                            estimate = predicted,
                            matches("^Class"))
  )
  expect_error(
    yardstick:::prob_select(two_class_example,
                            truth = truth,
                            predicted)
  )
  expect_error(
    yardstick:::prob_select(two_class_example,
                            truth = Class1,
                            Class2)
  )
  expect_error(
    yardstick:::prob_select(two_class_example,
                            truth = tru,
                            Class2)
  )  
  expect_error(
    yardstick:::prob_select(two_class_example,
                            truth = truth,
                            Class)
  )   
})

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
