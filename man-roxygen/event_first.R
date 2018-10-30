#' @section Relevant level:
#'
#' There is no common convention on which factor level should
#' automatically be considered the "event" or "positive" results.
#' In `yardstick`, the default is to use the _first_ level. To
#' change this, a global option called `yardstick.event_first` is
#' set to `TRUE` when the package is loaded. This can be changed
#' to `FALSE` if the last level of the factor is considered the
#' level of interest.
