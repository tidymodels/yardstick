#' @examples
#' count_truth <- c(2L,   7L,   1L,   1L,   0L,  3L)
#' count_pred  <- c(2.14, 5.35, 1.65, 1.56, 1.3, 2.71)
#' count_results <- dplyr::tibble(count = count_truth, pred = count_pred)
#'
#' # Supply truth and predictions as bare column names
#' <%=fn %>(count_results, count, pred)
#'

