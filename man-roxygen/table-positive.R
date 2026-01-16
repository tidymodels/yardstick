#' @section Implementation:
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{\text{Sensitivity} = \frac{A}{A + C}}
#'
#' \deqn{\text{Specificity} = \frac{D}{B + D}}
#'
#' \deqn{\text{Prevalence} = \frac{A + C}{A + B + C + D}}
#'
#' \deqn{\text{PPV} = \frac{\text{Sensitivity} \cdot \text{Prevalence}}{(\text{Sensitivity} \cdot \text{Prevalence}) + ((1 - \text{Specificity}) \cdot (1 - \text{Prevalence}))}}
#'
#' \deqn{\text{NPV} = \frac{\text{Specificity} \cdot (1 - \text{Prevalence})}{((1 - \text{Sensitivity}) \cdot \text{Prevalence}) + ((\text{Specificity}) \cdot (1-Prevalence))}}
#'
#' See the references for discussions of the statistics.
