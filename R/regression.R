#' Calculate Metrics for Numerical Data
#'
#'
#' rsq differences, equations
#'
#'
#' @param data A data frame
#' @param truth A single character value containing the column
#'  name of `data` that contains the true values.
#' @param estimate A single character value containing the column
#'  name of `data` that contains the predicted values.
#' @param bias A logical; should the biased estimate of variance
#'  be used for the concordance correlation coefficient (as is
#'  the original formula)?
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#' @param ... Not currently used.
#' @return A number 
#' @author Max Kuhn
#' @references Kvalseth. Cautionary note about \eqn{R^2}. 
#' American Statistician (1985) vol. 39 (4) pp. 279-285
#' @keywords manip
#' @export 
#' @rdname rmse
rmse <- function(data, ...)
  UseMethod("rmse")


## TODO I'm using `get_col` since the data might be tibbles and
## will not drop down to a vector. A better method is prob out
## there

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases 
rmse.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    sqrt( mean( (get_col(data, estimate) - get_col(data, truth)) ^ 2) )
  }

#' @export 
#' @rdname rmse
rsq <- function(data, ...)
  UseMethod("rsq")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases cor
rsq.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    cor(data)[1,2]
  }


#' @export 
#' @rdname rmse
rsq_trad <- function(data, ...)
  UseMethod("rsq_trad")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases var
rsq_trad.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    n <- nrow(data)
    ss <- sum( (get_col(data, estimate) - get_col(data, truth)) ^ 2)
    1 - (ss / ((n - 1) * var(get_col(data, truth))))
  }


#' @export 
#' @rdname rmse
mae <- function(data, ...)
  UseMethod("mae")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases
mae.data.frame <-
  function(data, truth = NULL, estimate = NULL, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    mean( abs(get_col(data, estimate) - get_col(data, truth)) )
  }


#' @export 
#' @rdname rmse
ccc <- function(data, ...)
  UseMethod("ccc")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases var
ccc.data.frame <-
  function(data, truth = NULL, estimate = NULL, bias = FALSE, na.rm = TRUE, ...) {
    check_call_vars(match.call(expand.dots = TRUE))
    data <- data[, c(truth, estimate)]
    if (na.rm)
      data <- data[complete.cases(data),]
    
    m_e <- mean(get_col(data, estimate))
    m_t <- mean(get_col(data, truth))
    v_e <- var(get_col(data, estimate))
    v_t <- var(get_col(data, truth))
    cross <- scale(get_col(data, truth), scale = FALSE) *
      scale(get_col(data, estimate), scale = FALSE)
    cross <- mean(cross)
    
    if (bias) {
      n <- nrow(data)
      v_e <- v_e * (n - 1) / n
      v_t <- v_t * (n - 1) / n
    }
    
    2 * cross / (v_e + v_t + (m_e - m_t) ^ 2)
  }
