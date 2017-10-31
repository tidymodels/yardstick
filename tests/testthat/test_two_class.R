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
})


###################################################################

test_that('bad args', {
  expect_error(sens(ab_df, truth = ab_df$pathology, estimate = ab_df$scan))
  expect_error(sens(ab_df, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(ab_df, truth = "patholosgy", estimate = "scan"))
 
})