#' @section Multiclass:
#'
#' If a multiclass `truth` column is provided, a one-vs-all
#' approach will be taken to calculate multiple curves, one per level.
#' In this case, there will be an additional column, `.level`,
#' identifying the "one" column in the one-vs-all calculation.
