#' @examples
#' # Two class
#' data("two_class_example")
#' <%=fn %>(two_class_example, truth, predicted)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   <%=fn %>(obs, pred)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   <%=fn %>(obs, pred)
#'
#' # Weighted macro averaging
#' hpc_cv |>
#'   group_by(Resample) |>
#'   <%=fn %>(obs, pred, estimator = "macro_weighted")
#'
#' # Vector version
#' <%=fn %>_vec(
#'   two_class_example$truth,
#'   two_class_example$predicted
#' )
#'
#' # Making Class2 the "relevant" level
#' <%=fn %>_vec(
#'   two_class_example$truth,
#'   two_class_example$predicted,
#'   event_level = "second"
#' )
