#' @examples
#' # You can use the col1:colN tidyselect syntax
#' library(dplyr)
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   <%=metric_fn %>(obs, VF:L)
#'
#' # Change the first level of obs variable from "VF" to "M" and thus the
#' # event of interest is now "obs" == "M".
#' library(forcats)
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   mutate(obs = fct_relevel(obs, "M")) %>%
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
