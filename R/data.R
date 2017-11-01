#' Liver Pathology Data
#'
#' @details These data have the results of a _x_-ray examination
#'  to determine whether liver is abnormal or not (in the `scan`
#'  column) versus the more extensive pathology results that
#'  approximate the truth (in `pathology`).
#'
#' @name pathology
#' @aliases pathology
#' @docType data
#' @return \item{pathology}{a data frame}
#'
#' @source Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#'  sensitivity and specificity,'' *British Medical Journal*,
#'  vol 308, 1552.
#'
#'
#' @keywords datasets
#' @examples
#' data(pathology)
#' str(pathology)
NULL

#' Solubility Predictions from MARS Model
#'
#' @details For the solubility data in Kuhn and Johnson (2013), 
#'  these data are the test set results for the MARS model. The
#'  observed solubility (in column `solubility`) and the model
#'  results (`prediction`) are contained in the data. 
#'
#' @name solubility_test
#' @aliases solubility_test
#' @docType data
#' @return \item{solubility_test}{a data frame}
#'
#' @source Kuhn, M., Johnson, K. (2013) *Applied Predictive 
#'  Modeling*, Springer
#'
#' @keywords datasets
#' @examples
#' data(solubility_test)
#' str(solubility_test)
NULL


#' Class Probability Predictions
#'
#' @details This data frame contains the predicted classes and
#'  class probabilities for a linear discriminant analysis model fit
#'  to the HPC data set from Kuhn and Johnson (2013). These data are
#'  the assessment sets from a 10-fold cross-validation scheme. The
#'  data column columns for the true class (`obs`), the class
#'  prediction (`pred`) and columns for each class probability
#'  (columns `VF`, `F`, `M`, and `L`). Additionally, a column for
#'  the resample indicator is included.
#'
#' @name hpc_cv
#' @aliases hpc_cv
#' @docType data
#' @return \item{hpc_cv}{a data frame}
#'
#' @source Kuhn, M., Johnson, K. (2013) *Applied Predictive 
#'  Modeling*, Springer
#'
#' @keywords datasets
#' @examples
#' data(hpc_cv)
#' str(hpc_cv)
NULL


#' Two Class Predictions
#'
#' @details These data are a test set form a model built for two
#'  classes ("Class1" and "Class2"). There are columns for the true
#'  and predicted classes and column for the probabilities for each
#'  class.
#'
#' @name two_class_example
#' @aliases two_class_example
#' @docType data
#' @return \item{two_class_example}{a data frame}
#'
#' @keywords datasets
#' @examples
#' data(two_class_example)
#' str(two_class_example)
NULL

