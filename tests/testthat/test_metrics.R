library(testthat)
library(yardstick)
library(dplyr)

set.seed(1311)
three_class <- data.frame(obs = iris$Species,
                          pred = sample(iris$Species, replace = TRUE))
probs <- matrix(runif(150 * 3), nrow = 150)
probs <- t(apply(probs, 1, function(x) x/sum(x)))
colnames(probs) <- levels(iris$Species)
three_class <- cbind(three_class, as.data.frame(probs))


###################################################################

test_that('correct metrics returned', {
  expect_equal(
    names(metrics(two_class_example, truth, predicted)),
    c("accuracy", "kappa")
  )
  expect_equal(
    names(metrics(two_class_example, truth, predicted, starts_with("Class"))),
    c("accuracy", "kappa", "mnLogLoss", "roc_auc")
  )
  expect_equal(
    names(metrics(three_class, "obs", "pred", setosa, versicolor, virginica)),
    c("accuracy", "kappa", "mnLogLoss")
  )
  expect_equal(
    names(metrics(solubility_test, solubility, "prediction")),
    c("rmse", "rsq", "mae")
  )
})

###################################################################

test_that('bad args', {
  expect_error(
    metrics(two_class_example, truth, Class1)
  )
  expect_error(
    metrics(two_class_example, Class1, truth)
  )
  expect_error(
    metrics(three_class, "obs", "pred", setosa, versicolor)
  )
  expect_error(
    metrics(two_class_example, truth, predicted, Class1)
  )
})


###################################################################

test_that('bad args', {
  expect_error(
    metrics(two_class_example, truth, Class1)
  )
  expect_error(
    metrics(two_class_example, Class1, truth)
  )
  expect_error(
    metrics(three_class, "obs", "pred", setosa, versicolor)
  )
  expect_error(
    metrics(two_class_example, truth, predicted, Class1)
  )
})

###################################################################

class_res_1 <-
  tibble(
    accuracy = accuracy(two_class_example, truth, predicted),
    kappa = kap(two_class_example, truth, predicted),
    mnLogLoss = mnLogLoss(two_class_example, truth,
                          Class1, Class2),
    roc_auc = roc_auc(two_class_example, truth,
                      Class1)
  )

reg_res_1 <-
  tibble(
    rmse = rmse(solubility_test, solubility, "prediction"),
    rsq = rsq(solubility_test, solubility, prediction),
    mae = mae(solubility_test, solubility, prediction)
  )

test_that('correct results', {
  expect_equal(
    metrics(two_class_example, truth, predicted),
    class_res_1[, c("accuracy", "kappa")]
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1, Class2),
    class_res_1
  )
  expect_equal(
    metrics(solubility_test, solubility, prediction),
    reg_res_1
  )
})
