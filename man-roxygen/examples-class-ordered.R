#' @examples
#' # Two class
#' data("two_class_example")
#' two_class_example$truth <- factor(
#'   two_class_example$truth, levels(two_class_example$truth), ordered = TRUE
#' )
#' two_class_example$predicted <- factor(
#'   two_class_example$predicted, levels(two_class_example$truth), ordered = TRUE
#' )
#' <%=metric_fn %>(two_class_example, truth, predicted)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#' hpc_cv$obs <- factor(
#'   hpc_cv$obs, levels(hpc_cv$obs), ordered = TRUE
#' )
#' hpc_cv$pred <- factor(
#'   hpc_cv$pred, levels(hpc_cv$obs), ordered = TRUE
#' )
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
