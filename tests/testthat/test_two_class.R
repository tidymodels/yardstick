library(testthat)
library(yardstick)
library(pROC)
library(MLmetrics)
library(tidyselect)

## data from: Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#'  sensitivity and specificity,'' *British Medical Journal*,
#'  vol 308, 1552.

data(pathology)

pathology$scan_ch <- as.character(pathology$scan)
pathology$scan_na <- pathology$scan
pathology$scan_na[c(1, 250, 300)] <- NA

path_tbl <- as.table(matrix(c(231, 27, 32, 54), ncol = 2))
rownames(path_tbl) <- levels(pathology$pathology)
colnames(path_tbl) <- levels(pathology$pathology)

###################################################################

# Data in Table 2 of Powers (2007)

pr_lvs <- c("Relevant", "Irrelevant")

tbl_2_1_pred <- factor(rep(pr_lvs, times = c(42, 58)), levels = pr_lvs)
tbl_2_1_pred2 <- tbl_2_1_pred
tbl_2_1_pred2[c(1, 10, 20, 30, 40, 50)] <- NA
tbl_2_1_truth <- factor(c(rep(pr_lvs, times = c(30, 12)),
                          rep(pr_lvs, times = c(30, 28))),
                        levels = pr_lvs)
tbl_2_1 <- table(tbl_2_1_pred, tbl_2_1_truth)
df_2_1 <- data.frame(truth  = tbl_2_1_truth,
                     prediction = tbl_2_1_pred,
                     pred_na = tbl_2_1_pred2)

###################################################################

test_that('global var', {
  expect_true("yardstick.event_first" %in% names(options()))
  expect_true(getOption("yardstick.event_first"))
  options(yardstick.event_first = FALSE)
  expect_false(getOption("yardstick.event_first"))
  options(yardstick.event_first = TRUE)
})

pred_ch <- quote(scan)

###################################################################

test_that('sensitivity', {
  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan"),
    231/258
  )
  expect_equal(
    sens(pathology, estimate = scan, truth = pathology),
    231/258
  )
  expect_equal(
    sens(pathology, pathology, !! pred_ch),
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan),
    231/258
  )
  expect_equal(
    sens(path_tbl),
    231/258
  )
  expect_equal(
    sens(pathology, truth = pathology, estimate = "scan_na"),
    230/256
  )
  expect_equal(
    sens(as.matrix(path_tbl)),
    231/258
  )
})


test_that('specificity', {
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan"),
    54/86
  )
  expect_equal(
    spec(path_tbl),
    54/86
  )
  expect_equal(
    spec(pathology, truth = pathology, estimate = "scan_na"),
    53/85
  )
  expect_equal(
    spec(as.matrix(path_tbl)),
    54/86
  )
})


test_that('ppv', {
  expect_equal(
    ppv(pathology, truth = "pathology", estimate = "scan"),
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(path_tbl),
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan_na"),
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan", prevalence = .5),
    0.70642,
    tolerance = .001
  )
})

test_that('npv', {
  expect_equal(
    npv(pathology, truth = "pathology", estimate = "scan"),
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(path_tbl),
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan_na"),
    0.67088,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan", prevalence = .5),
    0.85714,
    tolerance = .001
  )
})


test_that('recall', {
  expect_equal(
    recall(df_2_1, truth = "truth", estimate = "prediction"),
    30/60
  )
  expect_equal(
    recall(tbl_2_1),
    30/60
  )
  expect_equal(
    recall(df_2_1, truth = truth, estimate = pred_na),
    26/(26+29)
  )
})


test_that('precision', {
  expect_equal(
    precision(df_2_1, truth = "truth", estimate = "prediction"),
    30/42
  )
  expect_equal(
    precision(tbl_2_1),
    30/42
  )
  expect_equal(
    precision(df_2_1, truth = truth, estimate = pred_na),
    26/37
  )
})


test_that('F1', {
  expect_equal(
    f_meas(df_2_1, truth = "truth", estimate = "prediction"),
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(tbl_2_1),
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(df_2_1, truth = truth, estimate = pred_na),
    0.5652174,
    tol = 0.0001
  )
})


test_that('Matthews correlation coefficient', {
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan"),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(path_tbl),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(pathology, truth = pathology, estimate = scan_na),
    ((230 * 53) - (32 * 26)) / sqrt((230 + 32)*(230 + 26) * (53 + 32) * (53 + 26))
  )
})


test_that('Youden J', {
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan"),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(path_tbl),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(pathology, pathology, scan),
    (231/258) + (54/86)  - 1
  )
})



test_that('Balanced Accuracy', {
  expect_equal(
    bal_accuracy(pathology, truth = "pathology", estimate = "scan"),
    ( sens(path_tbl) + spec(path_tbl) )/2
  )
  expect_equal(
    bal_accuracy(path_tbl),
    ( sens(path_tbl) + spec(path_tbl) )/2
  )
  expect_equal(
    bal_accuracy(pathology, pathology, scan),
    ( sens(path_tbl) + spec(path_tbl) )/2
  )
})



test_that('Detection Prevalence', {
  expect_equal(
    detection_prevalence(pathology, truth = "pathology", estimate = "scan"),
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(path_tbl),
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(pathology, pathology, scan),
    ( 231 + 32 ) / 344
  )
})

###################################################################

roc_curv <- pROC::roc(two_class_example$truth,
                      two_class_example$Class1,
                      levels = rev(levels(two_class_example$truth)))
lvls <- levels(two_class_example$truth)
roc_val <- as.numeric(roc_curv$auc)
smooth_curv <- pROC::roc(two_class_example$truth,
                      two_class_example$Class1,
                      levels = rev(levels(two_class_example$truth)),
                      smooth = TRUE)

test_that('ROC AUC', {
  expect_equal(
    roc_auc(two_class_example, truth, Class1),
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth = "truth", starts_with("Class")),
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth = "truth", Class2),
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth, Class1, options = list(smooth = TRUE)),
    as.numeric(smooth_curv$auc),
    tol = 0.001
  )
})

test_that('ROC Curve', {
  library(pROC)
  points <- coords(roc_curv, x = unique(c(-Inf, two_class_example$Class1, Inf)), input = "threshold")
  points <- dplyr::as_tibble(t(points)) %>% dplyr::arrange(threshold)
  s_points <- coords(smooth_curv, x = unique(c(0, smooth_curv$specificities, 1)), input = "specificity")
  s_points <- dplyr::as_tibble(t(s_points)) %>% dplyr::arrange(specificity)

  expect_equal(
    as.data.frame(roc_curve(two_class_example, truth, Class1)),
    as.data.frame(points)
  )
  expect_equal(
    as.data.frame(roc_curve(two_class_example, truth, Class1, options = list(smooth = TRUE))),
    as.data.frame(s_points)
  )
})

pr_val <-
  MLmetrics::PRAUC(two_class_example$Class1,
                   ifelse(two_class_example$truth == lvls[1], 1, 0))

test_that('PR Curve', {
  expect_equal(
    pr_auc(two_class_example, truth = "truth", !! lvls),
    pr_val
  )
  expect_equal(
    pr_auc(two_class_example, truth,  Class1),
    pr_val
  )
})

ll_dat <- data.frame(
  obs  = c("A", "A", "A", "B", "B", "C"),
  A = c(1, .80, .51, .1, .2, .3),
  B = c(0, .05, .29, .8, .6, .3),
  C = c(0, .15, .20, .1, .2, .4)
  )

test_that('LogLoss', {
  expect_equal(
    mnLogLoss(ll_dat, obs, LETTERS[1:3]),
    (log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4))/6
  )
  expect_equal(
    mnLogLoss(ll_dat, truth = "obs", A, B, C, sum = TRUE),
    log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4)
  )
})

###################################################################

test_that('switch event definition', {
  options(yardstick.event_first = FALSE)
  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan"),
    54/86
  )
  expect_equal(
    sens(path_tbl),
    54/86
  )
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan"),
    231/258
  )
  expect_equal(
    spec(path_tbl),
    231/258
  )
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan"),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan"),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
})

###################################################################

test_that('bad args', {
  expect_error(sens(pathology, truth = pathology$pathology, estimate = pathology$scan))
  expect_error(sens(pathology, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(pathology, truth = "patholosgy", estimate = "scan"))

})
