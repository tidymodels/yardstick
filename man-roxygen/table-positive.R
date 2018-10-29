#' @section Implementation:
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{Sensitivity = A/(A+C)}
#' \deqn{Specificity = D/(B+D)}
#' \deqn{Prevalence = (A+C)/(A+B+C+D)}
#' \deqn{PPV = (Sensitivity * Prevalence) / ((Sensitivity * Prevalence) + ((1-Specificity) * (1-Prevalence)))}
#' \deqn{NPV = (Specificity * (1-Prevalence)) / (((1-Sensitivity) * Prevalence) + ((Specificity) * (1-Prevalence)))}
#'
#' See the references for discussions of the statistics.
