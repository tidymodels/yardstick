#' @examples
#' # ---------------------------------------------------------------------------
#' # Two class example
#'
#' # `truth` is a 2 level factor. The first level is `"Class1"`, which is the
#' # "event of interest" by default in yardstick. See the Relevant Level
#' # section above.
#' data(two_class_example)
#'
#' # Binary metrics using class probabilities take a factor `truth` column,
#' # and a single class probability column containing the probabilities of
#' # the event of interest. Here, since `"Class1"` is the first level of
#' # `"truth"`, it is the event of interest and we pass in probabilities for it.
#' <%=fn %>(two_class_example, truth, Class1)
#'
