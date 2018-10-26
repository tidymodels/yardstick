#' @examples
#' # Two class
#' data("two_class_example")
#' <%=metric_fn %>(two_class_example, truth, Class1)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   <%=metric_fn %>(obs, VF:L)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   <%=metric_fn %>(obs, VF:L)
#'
#' # Weighted macro averaging
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   <%=metric_fn %>(obs, VF:L, estimator = "macro_weighted")
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv %>%
#'   filter(Resample == "Fold01")
#'
#' <%=metric_fn %>_vec(
#'    truth = fold1$obs,
#'    matrix(
#'      c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'      ncol = 4
#'    )
#' )
#'
