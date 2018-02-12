library(testthat)
library(yardstick)

set.seed(1311)
three_class <- data.frame(obs = iris$Species,
                          pred = sample(iris$Species, replace = TRUE),
                          pred_na = sample(iris$Species))
three_class$pred_na[sample.int(150, 10)] <- NA
three_class$pred_ch <- as.character(three_class$pred)
three_class$pred_lvl <- factor(as.character(three_class$pred),
                               levels = rev(levels(iris$Species)))
three_class$pred_val <- three_class$pred_ch
three_class$pred_val[1] <- "wrong_value"

three_class_tb <- table(three_class$obs, three_class$pred)


###################################################################

test_that('confusion matrix', {
  expect_equivalent(
   conf_mat(three_class, truth = "obs", estimate = "pred")$table,
   three_class_tb
 )
})

test_that('flat tables', {
  expect_equivalent(
    yardstick:::flatten(three_class_tb),
    as.vector(three_class_tb)
  )
  expect_equal(
    names(yardstick:::flatten(three_class_tb[1:2, 1:2])),
    c("cell_1_1", "cell_2_1", "cell_1_2", "cell_2_2")
  )
  expect_error(yardstick:::flatten(three_class_tb[, 1:2]))
})

test_that('confusion matrix statistics', {
  sum_obj_3 <- summary(conf_mat(three_class, obs, pred),
                       wide = TRUE)
  sum_obj_2 <- summary(conf_mat(three_class_tb[1:2, 1:2]),
                       wide = TRUE)
  expect_equal(
    names(sum_obj_3),
    c("accuracy", "kappa")
  )
  expect_equal(
    sum_obj_3$accuracy,
    accuracy(three_class_tb)
  )
  expect_equal(
    names(sum_obj_2),
    c('accuracy', 'kappa', 'sens', 'spec', 'prevalence', 'ppv',
      'npv', 'mcc', 'j_index', 'precision', 'recall', 'F1')
  )
  expect_equal(
    sum_obj_2$sens,
    sens(three_class_tb[1:2, 1:2])
  )
})


###################################################################


test_that('accuracy', {
  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred"),
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class_tb),
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(as.matrix(three_class_tb)),
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class, obs, pred_na),
    (11 + 10 + 11)/140
  )
})

###################################################################

test_that('name/col matching', {
  expect_error(
    yardstick:::match_levels_to_cols(names(iris))
  )
  expect_equal(
    yardstick:::match_levels_to_cols(
      nms = c("Class1", "Class2"),
      lvl = c("Class1", "Class2")
    ),
    "Class1"
  )
  expect_equal(
    yardstick:::match_levels_to_cols(
      nms = "Class1",
      lvl = c("Class1", "Class2")
    ),
    "Class1"
  )
  expect_equal(
    yardstick:::match_levels_to_cols(
      nms = "Class1",
      lvl = c("Class2", "Class1")
    ),
    "Class1"
  )
  expect_equal(
    yardstick:::match_levels_to_cols(
      nms = c("Class1", "Class2"),
      lvl = c("Class2", "Class1")
    ),
    "Class2"
  )
  expect_error(
    yardstick:::match_levels_to_cols(
      nms = c("Class 1", "Class 2"),
      lvl = c("Class2", "Class1")
    )
  )
})






