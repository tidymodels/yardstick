# Data in Table 2 of Powers (2007)

data_powers <- function() {
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

  list(tabl_2_1 = tbl_2_1, df_2_1 = df_2_1)
}
