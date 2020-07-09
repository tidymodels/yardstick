#' @section Relevant Level:
#'
#' There is no common convention on which factor level should
#' automatically be considered the "event" or "positive" result
#' when computing binary classification metrics. In `yardstick`, the default
#' is to use the _first_ level. To alter this, change the argument
#' `event_level` to `"second"` to consider the _last_ level of the factor the
#' level of interest. For multiclass extensions involving one-vs-all
#' comparisons (such as macro averaging), this option is ignored and
#' the "one" level is always the relevant result.
