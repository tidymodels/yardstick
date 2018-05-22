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
#' data("surv_example")
#'
#' # Given a sample
#' require(survival)
#' mod <- coxph(Surv(time, status)~ age, data = surv_example)
#'
#' tdroc(surv_example, mod)
#'
#' @export tdroc
#' @author Carlos S Traynor
#' @references
#'
#'  Liang Li, Cai Wu Department of Biostatistics and
#'  The University of Texas MD Anderson Cancer Center (2016).
#'  tdROC: Nonparametric Estimation of Time-Dependent ROC Curve
#'   from Right Censored Survival Data. R package version 1.0.
#'  https://CRAN.R-project.org/package=tdROC
#' @export tdroc
#'
tdroc <- function(data, mod,...)
  UseMethod("tdroc")

#' @export
#' @rdname tdroc

"tdroc.model.list" <-
  function(data, mod, ...) {
    pred_dat <- assessment(data)
    probs <- predict(mod, newdata = pred_dat, type = "lp")
    roc <- tdROC::tdROC(X = probs,
                        Y = pred_dat$os_months,
                        delta = pred_dat$os_deceased,
                        tau = quantile(pred_dat$os_months, .9),
                        n.grid = 1000)
    return(roc)
  }
#' @export
#' @rdname tdroc
"tdroc.int.matrix" <-
  function(data, mod, ...) {
    model <- tdroc.model.list(data, mod)
    iroc <- model$AUC[1] %>% unlist
    return(iroc)
  }

