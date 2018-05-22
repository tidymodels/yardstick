#' Get c-index
#'
#'
#' The efficacy of the survival model can be measured by
#'  the concordance statistic
#'
#' @param data For the default functions, a datframe containing survival
#' (time), and status (0:censored/1:event), and the explanatory variables.
#' @param mod Coxph model object fitted with coxph (survival).
#' @return A cindex object
#' @seealso [coxph]
#' @keywords cindex
#' @examples
#' data("surv_example")
#'
#' # Given a sample
#' require(survival)
#' mod <- coxph(Surv(time, status)~ age, data = surv_example)
#'
#' cindex(surv_example, mod)
#'
#' @export get_cindex
#' @author Carlos S Traynor
#' @references
#'
#'  Terry M. Therneau and Patricia M. Grambsch (2000).
#'   _Modeling Survival Data: Extending the Cox Model_.
#'   Springer, New York. ISBN 0-387-98784-3.
#'   @export get_cindex
#'
get_cindex <- function(data, mod,...)
  UseMethod("get_cindex")

#' @export
#' @rdname get_cindex
get_cindex <-
  function(data, mod, ...) {
    pred_dat <- assessment(data)
    pred_dat$pred <- predict(mod, newdata = pred_dat)
    survival::survConcordance(Surv(os_months, os_deceased) ~ pred, pred_dat, ...)$concordance
  }




