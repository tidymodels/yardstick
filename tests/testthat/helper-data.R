# Data in Table 2 of Powers (2007)

data_powers <- function() {
  pr_lvs <- c("Relevant", "Irrelevant")

  tbl_2_1_pred <- factor(rep(pr_lvs, times = c(42, 58)), levels = pr_lvs)
  tbl_2_1_pred2 <- tbl_2_1_pred
  tbl_2_1_pred2[c(1, 10, 20, 30, 40, 50)] <- NA
  tbl_2_1_truth <- factor(
    c(
      rep(pr_lvs, times = c(30, 12)),
      rep(pr_lvs, times = c(30, 28))
    ),
    levels = pr_lvs
  )
  tbl_2_1 <- table(tbl_2_1_pred, tbl_2_1_truth)
  df_2_1 <- data.frame(
    truth = tbl_2_1_truth,
    prediction = tbl_2_1_pred,
    pred_na = tbl_2_1_pred2
  )

  list(tabl_2_1 = tbl_2_1, df_2_1 = df_2_1)
}

data_altman <- function() {
  ## data from: Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
  #'  sensitivity and specificity,'' *British Medical Journal*,
  #'  vol 308, 1552.

  data(pathology)

  pathology$scan_ch <- as.character(pathology$scan)
  pathology$scan_na <- pathology$scan
  pathology$scan_na[c(1, 250, 300)] <- NA

  path_tbl <- as.table(matrix(c(231L, 27L, 32L, 54L), ncol = 2))
  rownames(path_tbl) <- levels(pathology$pathology)
  colnames(path_tbl) <- levels(pathology$pathology)

  list(pathology = pathology, path_tbl = path_tbl)
}

# Helper data for 3 class example generation i.e. data_three_class().
# Static results since we cannot rely on sample()
# to give us the same values post R 3.6. In <3.6 they were generated with:

# set.seed(1311)
# three_class_helpers <- list(
#   pred = sample(iris$Species, replace = TRUE),
#   pred_na = sample(iris$Species),
#   where_na = sample.int(150, 10)
# )
# saveRDS(three_class_helpers, testthat::test_path("data/helper-three-class-helpers.rds"))

data_three_class <- function() {
  three_class_helpers <- readRDS(
    test_path("data/helper-three-class-helpers.rds")
  )

  three_class <- data.frame(
    obs = iris$Species,
    pred = three_class_helpers$pred,
    pred_na = three_class_helpers$pred_na
  )

  three_class$pred_na[three_class_helpers$where_na] <- NA
  three_class$pred_ch <- as.character(three_class$pred)

  three_class$pred_lvl <- factor(
    as.character(three_class$pred),
    levels = rev(levels(iris$Species))
  )

  three_class$pred_val <- three_class$pred_ch
  three_class$pred_val[1] <- "wrong_value"

  three_class_tb <- table(three_class$pred, three_class$obs)

  list(three_class = three_class, three_class_tb = three_class_tb)
}

## Build a small example with different class distributions, to test mlr result
# library(mlr)
# library(yardstick)
# library(rsample)
# library(dplyr)
# library(stringr)
# library(mlbench)
#
# data("Soybean")
#
# set.seed(123)
# soybean_split <- initial_split(Soybean)
#
# soybean_task = makeClassifTask(data = training(soybean_split),
#                                target = "Class")
#
# lrn <- makeLearner("classif.rpart", predict.type = "prob")
# mlr_mod <- train(lrn, soybean_task)
# mlr_mod
#
# pred <- predict(mlr_mod, newdata = testing(soybean_split))
#
# measures_mlr <- performance(pred,
#                             measures = list(multiclass.aunu,
#                                             multiclass.aunp))
# measures_mlr
# dput(measures_mlr)
# saveRDS(rename_at(pred$data, vars(starts_with("prob.")),
#                   ~ str_remove_all(., "prob.")),
#         testthat::test_path("data/helper-soybean.rds"),
#         version = 2, compress = "xz")

data_soybean <- function() {
  readRDS(test_path("data/helper-soybean.rds"))
}

# Made up data
data_counts <- function() {
  count_truth <- c(2L, 7L, 1L, 1L, 0L, 3L)
  count_pred <- c(2.14, 5.35, 1.65, 1.56, 1.3, 2.71)
  count_results <- dplyr::tibble(count = count_truth, pred = count_pred)

  count_poor <- count_results
  count_poor$pred <- 0.1

  count_missing <- count_results
  count_missing$pred[1] <- NA

  list(basic = count_results, missing = count_missing, poor = count_poor)
}

data_lung_surv <- function() {
  data("lung_surv")

  lung_surv
}
