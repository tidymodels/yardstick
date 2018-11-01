#' @examples
#' # Two class
#' data("two_class_example")
#' <%=metric_fn %>(two_class_example, truth, predicted)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   <%=metric_fn %>(obs, pred)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   <%=metric_fn %>(obs, pred)
#'
#' # Weighted macro averaging
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   <%=metric_fn %>(obs, pred, estimator = "macro_weighted")
#'
#' # Vector version
#' <%=metric_fn %>_vec(two_class_example$truth, two_class_example$predicted)
#'
#' # Making Class2 the "relevant" level
#' options(yardstick.event_first = FALSE)
#' <%=metric_fn %>_vec(two_class_example$truth, two_class_example$predicted)
#' options(yardstick.event_first = TRUE)
#'
