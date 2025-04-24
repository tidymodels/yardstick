#' @examples
#' # Supply truth and predictions as bare column names
#' <%=fn %>(solubility_test, solubility, prediction)
#'
#' library(dplyr)
#'
#' set.seed(1234)
#' size <- 100
#' times <- 10
#'
#' # create 10 resamples
#' solubility_resampled <- bind_rows(
#'   replicate(
#'     n = times,
#'     expr = sample_n(solubility_test, size, replace = TRUE),
#'     simplify = FALSE
#'   ),
#'   .id = "resample"
#' )
#'
#' # Compute the metric by group
#' metric_results <- solubility_resampled |>
#'   group_by(resample) |>
#'   <%=fn %>(solubility, prediction)
#'
#' metric_results
#'
#' # Resampled mean estimate
#' metric_results |>
#'   summarise(avg_estimate = mean(.estimate))
