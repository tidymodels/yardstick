#' @section Implementation:
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Relevant \tab
#' Irrelevant \cr Relevant \tab A \tab B \cr Irrelevant \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{recall = A/(A+C)}
#' \deqn{precision = A/(A+B)}
#' \deqn{F_{meas} = (1+\beta^2) * precision * recall/((\beta^2 * precision)+recall)}
#'
#' See the references for discussions of the statistics.
