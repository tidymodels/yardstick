#' @section Relevant Level:
#'
#' There is no common convention on which factor level should
#' automatically be considered the "event" or "positive" result.
#' In `yardstick`, the default is to use the _first_ level. To
#' change this, a global option called `yardstick.event_first` is
#' set to `TRUE` when the package is loaded. This can be changed
#' to `FALSE` if the _last_ level of the factor is considered the
#' level of interest by running: `options(yardstick.event_first = FALSE)`.
#' For multiclass extensions involving one-vs-all
#' comparisons (such as macro averaging), this option is ignored and
#' the "one" level is always the relevant result.
