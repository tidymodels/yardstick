#' @examples
#' # ---------------------------------------------------------------------------
#' # Multiclass example
#'
#' # `obs` is a 4 level factor. The first level is `"VF"`, which is the
#' # "event of interest" by default in yardstick. See the Relevant Level
#' # section above.
#' data(hpc_cv)
#'
#' # You can use the col1:colN tidyselect syntax
#' library(dplyr)
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   <%=fn %>(obs, VF:L)
#'
#' # Change the first level of `obs` from `"VF"` to `"M"` to alter the
#' # event of interest. The class probability columns should be supplied
#' # in the same order as the levels.
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   mutate(obs = relevel(obs, "M")) |>
#'   <%=fn %>(obs, M, VF:L)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   <%=fn %>(obs, VF:L)
#'
#' # Weighted macro averaging
#' hpc_cv |>
#'   group_by(Resample) |>
#'   <%=fn %>(obs, VF:L, estimator = "macro_weighted")
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv |>
#'   filter(Resample == "Fold01")
#'
#' <%=fn %>_vec(
#'    truth = fold1$obs,
#'    matrix(
#'      c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'      ncol = 4
#'    )
#' )
#'
