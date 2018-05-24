#' Calculate tdROC
#'
#'
#' The time dependent ROC analysis (tdROC) allows
#' the user to visualise the sensitivity (true positive rate) and
#' specificity (false positive rate) for all possible cut-offs for the
#' predicted survival.
#' #'
#' @param data For the default functions, a datframe containing survival
#' (time), and status (0:censored/1:event), and the explanatory variables.
#' @param mod Coxph model object fitted with coxph (survival).
#' @return A tdROC object
#' @seealso [iROC]
#' @keywords tdroc
#' @examples
#' require(survival)
#' require(dplyr)
#' data(lung)
#' lung <- lung %>%
#' mutate(status = (status == 2))
#'
#' mod <- coxph(Surv(time, status)~ age, data = lung)
#'
#' tdroc <- get_tdroc(lung, mod)
#' integrate_tdroc(tdroc)
#'
#' @export tdroc
#' @author Carlos S Traynor
#' @references
#'  Liang Li, Cai Wu Department of Biostatistics and
#'  The University of Texas MD Anderson Cancer Center (2016).
#'  tdROC: Nonparametric Estimation of Time-Dependent ROC Curve
#'   from Right Censored Survival Data. R package version 1.0.
#'  https://CRAN.R-project.org/package=tdROC
#' @export tdroc
tdroc <- function(data, mod,...)
  UseMethod("tdroc")

#' @export
#' @rdname tdroc
get_tdroc <-
  function(data, mod, ...) {
    pred_dat <- assessment(data)
    probs <- predict(mod, newdata = pred_dat, type = "lp")

    roc <- tdROC::tdROC(X = probs[!is.na(probs)],
                        Y = pred_dat$time[!is.na(probs)],
                        delta = pred_dat$status[!is.na(probs)],
                        tau = max(pred_dat$time),
                        n.grid = 1000)
    return(roc)
  }
#' @export
#' @rdname tdroc
integrate_tdroc  <-
  function( mod, ...) {
     mod$AUC[1] %>% unlist
  }

