context("Classes")

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

three_class_tb <- table(three_class$pred, three_class$obs)


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
  sum_obj_3 <- summary(conf_mat(three_class, obs, pred))
  sum_obj_2 <- summary(conf_mat(three_class_tb[1:2, 1:2]))
  expect_equal(
    sum_obj_3$.metric,
    c("accuracy", "kap")
  )
  expect_equal(
    dplyr::slice(sum_obj_3, 1),
    accuracy(three_class_tb)
  )
  expect_equal(
    sum_obj_2$.metric,
    c('accuracy', 'kap', 'sens', 'spec', 'prevalence', 'ppv',
      'npv', 'mcc', 'j_index', 'bal_accuracy', 'detection_prevalence',
      'precision', 'recall', 'f_meas')
  )
  expect_equal(
    dplyr::filter(sum_obj_2, .metric == "sens"),
    sens(three_class_tb[1:2, 1:2])
  )
})

###################################################################

test_that('accuracy', {
  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class_tb)[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(as.matrix(three_class_tb))[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class, obs, pred_na)[[".estimate"]],
    (11 + 10 + 11)/140
  )
  expect_equal(
    colnames(accuracy(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimate")
  )
  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "accuracy"
  )
})



###################################################################

# expected results from e1071::classAgreement(three_class_tb)$kappa
# e1071::classAgreement(table(three_class$pred_na, three_class$obs))$kappa

test_that('kappa', {
  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class_tb)[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(as.matrix(three_class_tb))[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class, obs, pred_na)[[".estimate"]],
    -0.1570248
  )
  expect_equal(
    colnames(kap(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimate")
  )
  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "kap"
  )
})

###################################################################

test_that('name/col matching', {
  expect_error(
    yardstick:::match_levels_to_cols(names(iris))
  )
  expect_equal(
    suppressWarnings(
      yardstick:::match_levels_to_cols(
        nms = c("Class1", "Class2"),
        lvl = c("Class1", "Class2")
      )
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
    suppressWarnings(
      yardstick:::match_levels_to_cols(
        nms = c("Class1", "Class2"),
        lvl = c("Class2", "Class1")
      )
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






