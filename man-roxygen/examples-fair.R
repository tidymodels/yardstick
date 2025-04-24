#' @examples
#' library(dplyr)
#'
#' data(hpc_cv)
#'
#' head(hpc_cv)
#'
#' # evaluate `<%=fn %>()` by Resample
#' m_set <- metric_set(<%=fn %>(Resample))
#'
#' # use output like any other metric set
#' hpc_cv |>
#'   m_set(truth = obs, estimate = pred)
#'
#' # can mix fairness metrics and regular metrics
#' m_set_2 <- metric_set(sens, <%=fn %>(Resample))
#'
#' hpc_cv |>
#'   m_set_2(truth = obs, estimate = pred)
