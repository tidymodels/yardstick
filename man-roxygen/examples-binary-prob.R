#' @examples
#' # Binary metrics using class probabilities take a factor `truth` column,
#' # and a single class probability column containing the probabilities of
#' # the event of interest. Here, since `"Class1"` is the first level of
#' # `"truth"`, it is the event of interest and we pass in probabilities for it.
#' <%=metric_fn %>(two_class_example, truth, Class1)
#'
