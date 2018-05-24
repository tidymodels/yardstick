#' Get Deviance
#'
#'
#' Information theory stablishes deviance as a measure of
#' uncertainty reduction. This function can be used to estimate the
#' out of sample deviance. Deviance should not be used when the
#' distribution is not the same across compared models.
#'
#' @param mod Coxph model object fitted with coxph (survival).
#' @return Deviance
#' @seealso [coxph]
#' @keywords deviance
#' @examples
#' require(survival)
#' require(dplyr)
#' data(lung)
#' lung <- lung %>%
#' mutate(status = (status == 2))
#'
#' mod <- coxph(Surv(time, status)~ age, data = lung)
#'
#' cox_deviance(mod)
#' @author Carlos S Traynor
#' @references
#'
#'  Terry M. Therneau and Patricia M. Grambsch (2000).
#'   _Modeling Survival Data: Extending the Cox Model_.
#'   Springer, New York. ISBN 0-387-98784-3.
#' @export cox_deviance
cox_deviance <-
  function( mod, ...) {
    -2*(mod$loglik)[2]
  }
