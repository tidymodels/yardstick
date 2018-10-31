context("Confusion matrix")

library(dplyr)

lst <- data_three_class()
three_class <- lst$three_class
three_class_tb <- lst$three_class_tb

###################################################################

test_that('Three class format', {
  expect_equivalent(
   conf_mat(three_class, truth = "obs", estimate = "pred")$table,
   three_class_tb
 )
})

test_that('Summary method', {
  sum_obj_3 <- summary(conf_mat(three_class, obs, pred))
  sum_obj_2 <- summary(conf_mat(three_class_tb[1:2, 1:2]))
  expect_equal(
    sum_obj_3$.metric,
    c("accuracy", "kap", "sens", "spec", "ppv", "npv", "mcc", "j_index",
      "bal_accuracy", "detection_prevalence", "precision", "recall",
      "f_meas")
  )
  expect_equal(
    dplyr::slice(sum_obj_3, 1),
    accuracy(three_class_tb)
  )
  expect_equal(
    sum_obj_2$.metric,
    c("accuracy", "kap", "sens", "spec", "ppv", "npv", "mcc", "j_index",
      "bal_accuracy", "detection_prevalence", "precision", "recall",
      "f_meas")
  )
  expect_equal(
    dplyr::filter(sum_obj_2, .metric == "sens"),
    sens(three_class_tb[1:2, 1:2])
  )
})

test_that('Summary method - estimators pass through', {
  sum_obj_micro <- summary(conf_mat(three_class, obs, pred), estimator = "micro")
  sum_obj_macrow <- summary(conf_mat(three_class, obs, pred), estimator = "macro_weighted")

  # All multiclass or micro
  expect_true(
    all(vapply(sum_obj_micro$.estimator, `%in%`, logical(1), c("multiclass", "micro")))
  )

  # All multiclass or macro_weighted
  expect_true(
    all(vapply(sum_obj_macrow$.estimator, `%in%`, logical(1), c("multiclass", "macro_weighted")))
  )
})

test_that("Grouped conf_mat() handler works", {

  hpc_g <- group_by(hpc_cv, Resample)
  res <- conf_mat(hpc_g, obs, pred)

  expect_is(res, "tbl_df")
  expect_is(res$conf_mat, "list")

  expect_equal(
    res$conf_mat[[1]],
    hpc_cv %>%
      filter(Resample == "Fold01") %>%
      conf_mat(obs, pred)
  )

})

test_that('Multilevel table -> conf_mat', {
  expect_equivalent(
    conf_mat(table(hpc_cv$pred, hpc_cv$obs)),
    conf_mat(hpc_cv, obs, pred)
  )
})

test_that('Multilevel matrix -> conf_mat', {
  expect_equivalent(
    conf_mat(as.matrix(table(hpc_cv$pred, hpc_cv$obs))),
    conf_mat(hpc_cv, obs, pred)
  )
})

test_that('Tidy method', {
  res <- tidy(conf_mat(hpc_cv, obs, pred))

  expect_equal(
    res$value[[1]],
    1620
  )

  expect_equal(
    res$name[[1]],
    "cell_1_1"
  )

})
