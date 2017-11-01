library(testthat)
library(yardstick)

## data from: Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#'  sensitivity and specificity,'' *British Medical Journal*,
#'  vol 308, 1552.

lev <- c("abnorm", "norm")
ab_df <- data.frame(
  pathology = c(
    rep("abnorm", 231),
    rep("abnorm", 27),
    rep("norm",   32),
    rep("norm",   54)
  ),
  scan =      c(
    rep("abnorm", 231),
    rep("norm",   27),
    rep("abnorm", 32),
    rep("norm",   54)
  )
)

ab_df$scan_ch <- ab_df$scan
ab_df$scan <- factor(ab_df$scan, levels = lev)
ab_df$scan_na <- ab_df$scan
ab_df$scan_na[c(1, 250, 300)] <- NA

ab_df$pathology <- factor(ab_df$pathology, levels = lev)
ab_tb <- as.table(matrix(c(231, 27, 32, 54), ncol = 2))
rownames(ab_tb) <- lev
colnames(ab_tb) <- lev

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

###################################################################

test_that('sensitivity', {
  expect_equal(
    sens(ab_df, truth = "pathology", estimate = "scan"),
    231/258
  )
  expect_equal(
    sens(ab_tb),
    231/258
  )
  expect_equal(
    sens(ab_df, truth = "pathology", estimate = "scan_na"),
    230/256
  )
  expect_equal(
    sens(as.matrix(ab_tb)),
    231/258
  )  
})


test_that('specificity', {
  expect_equal(
    spec(ab_df, truth = "pathology", estimate = "scan"),
    54/86
  )
  expect_equal(
    spec(ab_tb),
    54/86
  )  
  expect_equal(
    spec(ab_df, truth = "pathology", estimate = "scan_na"),
    53/85
  )
  expect_equal(
    spec(as.matrix(ab_tb)),
    54/86
  )    
})


test_that('ppv', {
  expect_equal(
    ppv(ab_df, truth = "pathology", estimate = "scan"),
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(ab_tb),
    0.87832,
    tolerance = .001
  )  
  expect_equal(
    ppv(ab_df, truth = "pathology", estimate = "scan_na"),
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(ab_df, truth = "pathology", estimate = "scan", prevalence = .5),
    0.70642,
    tolerance = .001
  )  
})

test_that('npv', {
  expect_equal(
    npv(ab_df, truth = "pathology", estimate = "scan"),
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(ab_tb),
    2/3,
    tolerance = .001
  )  
  expect_equal(
    npv(ab_df, truth = "pathology", estimate = "scan_na"),
    0.67088,
    tolerance = .001
  )
  expect_equal(
    npv(ab_df, truth = "pathology", estimate = "scan", prevalence = .5),
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
    recall(df_2_1, truth = "truth", estimate = "pred_na"),
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
    precision(df_2_1, truth = "truth", estimate = "pred_na"),
    26/37
  )
})


test_that('F1', {
  expect_equal(
    F_meas(df_2_1, truth = "truth", estimate = "prediction"),
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    F_meas(tbl_2_1),
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    F_meas(df_2_1, truth = "truth", estimate = "pred_na"),
    0.5652174,
    tol = 0.0001
  )
})


test_that('Matthews correlation coefficient', {
  expect_equal(
    mcc(ab_df, truth = "pathology", estimate = "scan"),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(ab_tb),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(ab_df, truth = "pathology", estimate = "scan_na"),
    ((230 * 53) - (32 * 26)) / sqrt((230 + 32)*(230 + 26) * (53 + 32) * (53 + 26))
  )
})


test_that('Youden J', {
  expect_equal(
    j_index(ab_df, truth = "pathology", estimate = "scan"),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    j_index(ab_tb),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    mcc(ab_df, truth = "pathology", estimate = "scan"),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
})


###################################################################

test_that('switch event definition', {
  options(yardstick.event_first = FALSE)
  expect_equal(
    sens(ab_df, truth = "pathology", estimate = "scan"),
    54/86
  )
  expect_equal(
    sens(ab_tb),
    54/86
  )  
  expect_equal(
    spec(ab_df, truth = "pathology", estimate = "scan"),
    231/258
  )
  expect_equal(
    spec(ab_tb),
    231/258
  )  
  expect_equal(
    j_index(ab_df, truth = "pathology", estimate = "scan"),
    (231/258) + (54/86)  - 1
  )
  expect_equal(
    mcc(ab_df, truth = "pathology", estimate = "scan"),
    ((231 * 54) - (32 * 27)) / sqrt((231 + 32)*(231 + 27) * (54 + 32) * (54 + 27))
  )
})

###################################################################

test_that('bad args', {
  expect_error(sens(ab_df, truth = ab_df$pathology, estimate = ab_df$scan))
  expect_error(sens(ab_df, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(ab_df, truth = "patholosgy", estimate = "scan"))
 
})