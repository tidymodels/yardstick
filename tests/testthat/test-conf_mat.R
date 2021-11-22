test_that('Three class format', {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

  expect_identical(
   conf_mat(three_class, truth = "obs", estimate = "pred", dnn = c("", ""))$table,
   three_class_tb
 )
})

test_that('Summary method', {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

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
  lst <- data_three_class()
  three_class <- lst$three_class

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

test_that("summary method - `event_level` passes through (#160)", {
  lst <- data_powers()
  df <- lst$df_2_1

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Irrelevant")
  df_rev$prediction <- relevel(df_rev$prediction, "Irrelevant")

  expect_equal(
    as.data.frame(summary(conf_mat(df, truth, prediction))),
    as.data.frame(summary(conf_mat(df_rev, truth, prediction), event_level = "second"))
  )
})

test_that("Grouped conf_mat() handler works", {

  hpc_g <- dplyr::group_by(hpc_cv, Resample)
  res <- conf_mat(hpc_g, obs, pred)

  expect_s3_class(res, "tbl_df")
  expect_type(res$conf_mat, "list")

  expect_equal(
    res$conf_mat[[1]],
    hpc_cv %>%
      dplyr::filter(Resample == "Fold01") %>%
      conf_mat(obs, pred)
  )

})

test_that('Multilevel table -> conf_mat', {
  expect_identical(
    conf_mat(table(hpc_cv$pred, hpc_cv$obs, dnn = c("Prediction", "Truth"))),
    conf_mat(hpc_cv, obs, pred)
  )
})

test_that('Multilevel matrix -> conf_mat', {
  expect_identical(
    conf_mat(as.matrix(table(hpc_cv$pred, hpc_cv$obs, dnn = c("Prediction", "Truth")))),
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
