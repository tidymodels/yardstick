#' @section Implementation:
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Relevant \tab
#' Irrelevant \cr Relevant \tab A \tab B \cr Irrelevant \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{\text{recall} = \frac{A}{A + C}}
#' \deqn{\text{precision} = \frac{A}{A + B}}
#' \deqn{F_{meas} = \frac{(1 + \beta^2) \cdot \text{precision} \cdot \text{recall}}{\beta^2 \cdot \text{precision} + \text{recall}}}
#'
#' See the references for discussions of the statistics.
