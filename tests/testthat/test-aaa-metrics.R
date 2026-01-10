set.seed(1311)
three_class <- data.frame(
  obs = iris$Species,
  pred = sample(iris$Species, replace = TRUE)
)
probs <- matrix(runif(150 * 3), nrow = 150)
probs <- t(apply(probs, 1, function(x) x / sum(x)))
colnames(probs) <- levels(iris$Species)
three_class <- cbind(three_class, as.data.frame(probs))

###################################################################

test_that("correct metrics returned", {
  expect_equal(
    metrics(two_class_example, truth, predicted)[[".metric"]],
    c("accuracy", "kap")
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1)[[".metric"]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[
      ".metric"
    ]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[
      ".estimator"
    ]],
    c("multiclass", "multiclass", "multiclass", "hand_till")
  )
  expect_equal(
    metrics(solubility_test, solubility, "prediction")[[".metric"]],
    c("rmse", "rsq", "mae")
  )
})

###################################################################

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    metrics(two_class_example, truth, Class1)
  )
  expect_snapshot(
    error = TRUE,
    metrics(two_class_example, Class1, truth)
  )
  expect_snapshot(
    error = TRUE,
    metrics(three_class, "obs", "pred", setosa, versicolor)
  )
})

###################################################################

class_res_1 <- dplyr::bind_rows(
  accuracy(two_class_example, truth, predicted),
  kap(two_class_example, truth, predicted),
  mn_log_loss(two_class_example, truth, Class1),
  roc_auc(two_class_example, truth, Class1)
)

reg_res_1 <- dplyr::bind_rows(
  rmse(solubility_test, solubility, "prediction"),
  rsq(solubility_test, solubility, prediction),
  mae(solubility_test, solubility, prediction)
)

test_that("correct results", {
  class_idx <- which(class_res_1$.metric %in% c("accuracy", "kap"))

  expect_equal(
    metrics(two_class_example, truth, predicted)[[".estimate"]],
    class_res_1[class_idx, ][[".estimate"]]
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1)[[".estimate"]],
    class_res_1[[".estimate"]]
  )
  expect_equal(
    metrics(solubility_test, solubility, prediction)[[".estimate"]],
    reg_res_1[[".estimate"]]
  )
})

###################################################################

test_that("metrics() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_snapshot({
    out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
  })

  expect_identical(
    out,
    metrics(two_class_example, truth, predicted, Class1)
  )
})
