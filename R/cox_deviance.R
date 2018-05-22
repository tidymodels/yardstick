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
#' data("surv_example")
#'
#' # Given a sample
#' require(survival)
#' mod <- coxph(Surv(time, status)~ age, data = surv_example)
#'
#' get_deviance(surv_example, mod)
#' @author Carlos S Traynor
#' @references
#'
#'  Terry M. Therneau and Patricia M. Grambsch (2000).
#'   _Modeling Survival Data: Extending the Cox Model_.
#'   Springer, New York. ISBN 0-387-98784-3.
#' @export get_deviance
get_deviance <-
  function( mod, ...) {
    -2*(mod$loglik)[2]
  }
