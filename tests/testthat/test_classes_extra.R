library(testthat)
library(yardstick)
library(pROC)
library(tidyselect)

context("Extra class metrics")

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
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, estimate = scan, truth = pathology)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, !! pred_ch)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    230/256
  )
  expect_equal(
    sens(as.matrix(path_tbl))[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan_na, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})


test_that('specificity', {
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    53/85
  )
  expect_equal(
    spec(as.matrix(path_tbl))[[".estimate"]],
    54/86
  )
})


test_that('ppv', {
  expect_equal(
    ppv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(path_tbl)[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[".estimate"]],
    0.70642,
    tolerance = .001
  )
})

test_that('npv', {
  expect_equal(
    npv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(path_tbl)[[".estimate"]],
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.67088,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[".estimate"]],
    0.85714,
    tolerance = .001
  )
})


test_that('recall', {
  expect_equal(
    recall(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30/60
  )
  expect_equal(
    recall(tbl_2_1)[[".estimate"]],
    30/60
  )
  expect_equal(
    recall(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26/(26+29)
  )
})


test_that('precision', {
  expect_equal(
    precision(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(tbl_2_1)[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26/37
  )
})


test_that('F1', {
  expect_equal(
    f_meas(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(tbl_2_1)[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    0.5652174,
    tol = 0.0001
  )
})


test_that('Matthews correlation coefficient', {
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(path_tbl)[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(pathology, truth = pathology, estimate = scan_na)[[".estimate"]],
    ((230 * 53) - (32 * 26)) / sqrt((230 + 32)*(230 + 26) * (53 + 32) * (53 + 26))
  )
})


test_that('Youden J', {
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(path_tbl)[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(pathology, pathology, scan)[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
})



test_that('Balanced Accuracy', {
  expect_equal(
    bal_accuracy(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
  expect_equal(
    bal_accuracy(path_tbl)[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
  expect_equal(
    bal_accuracy(pathology, pathology, scan)[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
})



test_that('Detection Prevalence', {
  expect_equal(
    detection_prevalence(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(path_tbl)[[".estimate"]],
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(pathology, pathology, scan)[[".estimate"]],
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
    roc_auc(two_class_example, truth, Class1)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth = "truth", Class2)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth, Class1, options = list(smooth = TRUE))[[".estimate"]],
    as.numeric(smooth_curv$auc),
    tol = 0.001
  )
})

test_that('ROC Curve', {
  library(pROC)
  points <- coords(roc_curv, x = unique(c(-Inf, two_class_example$Class1, Inf)), input = "threshold")
  points <- dplyr::as_tibble(t(points)) %>% dplyr::arrange(threshold) %>% dplyr::rename(.threshold = threshold)
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

# from:
# MLmetrics::PRAUC(two_class_example$Class1,
#                  ifelse(two_class_example$truth == lvls[1], 1, 0))
pr_val <- 0.942570731650901

test_that('PR AUC', {
  expect_equal(
    pr_auc(two_class_example, truth = "truth", "Class1")[[".estimate"]],
    pr_val
  )
  expect_equal(
    pr_auc(two_class_example, truth,  Class1)[[".estimate"]],
    pr_val
  )
})

# Known PR Curve result
pr_example <- data.frame(
  lab   = factor(c("Yes", "Yes", "No", "Yes"), levels = c("Yes", "No")),
  score = c(.9, .4, .35, .7)
)

pr_result <- list(
  .threshold = c(Inf, 0.9, 0.7, 0.4, 0.35),
  recall = c(0, 1/3, 2/3, 1, 1),
  precision = c(NA, 1, 1, 1, 0.75)
)

test_that('PR Curve', {
  expect_equal(
    as.list(pr_curve(pr_example, truth = "lab", "score")),
    pr_result
  )
})

ll_dat <- data.frame(
  obs  = c("A", "A", "A", "B", "B", "C"),
  A = c(1, .80, .51, .1, .2, .3),
  B = c(0, .05, .29, .8, .6, .3),
  C = c(0, .15, .20, .1, .2, .4)
  )

test_that('Log Loss', {
  expect_equal(
    mn_log_loss(ll_dat, obs, LETTERS[1:3])[[".estimate"]],
    -(log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4))/6
  )
  expect_equal(
    mn_log_loss(ll_dat, truth = "obs", A, B, C, sum = TRUE)[[".estimate"]],
    -(log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4))
  )

  # issue #29
  x <-
    structure(
      list(
        No = c(0.860384856004899, 1, 1),
        Yes = c(0.139615143995101, 0, 0),
        prob = c(0.139615143995101, 0, 0),
        estimate = structure(
          c(1L, 1L, 1L),
          .Label = c("No", "Yes"),
          class = "factor"
        ),
        truth = structure(
          c(2L, 1L, 2L),
          .Label = c("No", "Yes"),
          class = "factor"
        ),
        truth_num = c(1, 0, 1)
      ),
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_equal(
    mn_log_loss(x[1:2,], truth = truth, No)[[".estimate"]],
    0.9844328,
    tol = .0001
  )
  expect_equal(
    mn_log_loss(x, truth = truth, No)[[".estimate"]],
    0.6562885,
    tol = .0001
  )

})

x <- c(1, 1.2, 1.6, 2)
y <- c(4, 3.8, 4.2, 5)
# MLmetrics::Area_Under_Curve(x, y, "trapezoid")
auc_known <- 4.22

test_that('AUC', {
  expect_equal(auc(x, y), auc_known)
})

###################################################################

test_that('switch event definition', {
  options(yardstick.event_first = FALSE)
  on.exit(options(yardstick.event_first = TRUE))
  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54/86
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231/258
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    231/258
  )
  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
})

###################################################################

test_that('bad args', {
  expect_error(sens(pathology, truth = pathology$pathology, estimate = pathology$scan))
  expect_error(sens(pathology, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(pathology, truth = "patholosgy", estimate = "scan"))
})
