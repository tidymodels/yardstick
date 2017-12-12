#' Extra Metrics for Numeric Outcomes
#'
#' These functions are appropriate for cases where the model
#'  outcome is a number. The ratio of performance to deviation
#'  (`rpd`) and the ratio of performance to inter-quartile (`rpiq`)
#'  are both measures of consistency/correlation between observed
#'  and predicted values (and not of accuracy).
#'
#' @param data A data frame
#' @param truth The column identifier for the true results (that
#'  is numeric). This should an unquoted column name although this
#'  argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names or column positions).
#' @param estimate The column identifier for the predicted results
#'  (that is also numeric). As with `truth` this can be specified
#'  different ways but the primary method is to use an unquoted
#'  variable name.
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#' @param ... Not currently used.
#' @return A number or `NA`
#' @author Pierre Roudier
#' @details In the field of spectroscopy in particular, the ratio
#'  of performance to deviation (RPD) has been used as the standard
#'  way to report the quality of a model. It is the ratio between
#'  the standard deviation of a variable and the standard error of
#'  prediction of that variable by a given model. However, its
#'  systematic use has been criticized by several authors, since
#'  using the standard deviation to represent the spread of a
#'  variable can be misleading on skewed dataset. The ratio of
#'  performance to inter-quartile has been introduced by
#'  Bellon-Maurel et al. (2010) to address some of these issues, and
#'  generalise the RPD to non-normally distributed variables.
#'
#' @references
#'
#' Williams, P.C. (1987) Variables affecting near-infrared
#'  reflectance spectroscopic analysis. In: Near Infrared Technology
#'  in the Agriculture and Food Industries. 1st Ed. P.Williams and
#'  K.Norris, Eds. Am. Cereal Assoc. Cereal Chem., St. Paul, MN.

#'
#' Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger,
#'  J.M. and McBratney, A., (2010). Critical review of chemometric
#'  indicators commonly used for assessing the quality of the
#'  prediction of soil attributes by NIR spectroscopy. TrAC Trends
#'  in Analytical Chemistry, 29(9), pp.1073-1081.

#'
#' @keywords manip
#' @examples
#'
#' rpd(solubility_test, truth = solubility, estimate = prediction)
#' rpiq(solubility_test, truth = solubility, estimate = prediction)
#'
#' @export
#' @rdname rpd
rpd <- function(data, ...)
  UseMethod("rpd")

#' @rdname rpd
#' @export
#' @importFrom stats complete.cases sd
rpd.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    
    rpd_calc(data[[vars$truth]], data[[vars$estimate]])
  }

rpd_calc <- function(obs, pred)
  sd( obs ) / rmse_calc( obs, pred )

#' @export
#' @rdname rpd
rpiq <- function(data, ...)
  UseMethod("rpiq")

#' @rdname rpd
#' @export
#' @importFrom stats complete.cases IQR
rpiq.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    
    rpiq_calc(data[[vars$truth]], data[[vars$estimate]], na.rm = na.rm)
  }

rpiq_calc <- function(obs, pred, ...)
  IQR(obs, ...) / rmse_calc(obs, pred)
