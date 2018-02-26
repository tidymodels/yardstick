#' Calculate Metrics for Numeric Outcomes
#'
#' These functions are appropriate for cases where the model
#'  outcome is a number. The root mean squared error (`rmse`) and
#'  mean absolute error (`mae`) are error measures that are in the
#'  same units as the original data. The two estimates for the
#'  coefficient of determination, `rsq` and `rsq_trad`, differ by
#'  their formula. The former guarantees a value on (0, 1) while the
#'  latter can generate inaccurate values when the model is
#'  non-informative (see the examples). Both are measures of
#'  consistency/correlation and not of accuracy. The concordance
#'  correlation coefficient (`ccc`) is a measure of both. The mean absolute
#'  percent error (`mape`) is in relative units.
#'
#' @param data A data frame
#' @param truth The column identifier for the true results
#'  (that is numeric). This should an unquoted column name although
#'  this argument is passed by expression and support
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names or column positions).
#' @param estimate The column identifier for the predicted
#'  results (that is also numeric). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name.
#' @param bias A logical; should the biased estimate of variance
#'  be used for the concordance correlation coefficient (as is
#'  Lin (1989))?
#' @param na.rm A logical value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#' @param ... Not currently used.
#' @return A number or `NA`
#' @details Note that a value if `Inf` is returned for `mape` when the observed
#'  value is negative.
#' @author Max Kuhn
#' @references Kvalseth. Cautionary note about \eqn{R^2}.
#' American Statistician (1985) vol. 39 (4) pp. 279-285.
#'
#' Lin, L. (1989). A concordance correlation
#'  coefficient to evaluate reproducibility. _Biometrics_, 45 (1),
#'  255–268.
#'
#' Nickerson, C. (1997). A note on" A concordance correlation
#'  coefficient to evaluate reproducibility". _Biometrics_, 53(4),
#'  1503-1507.
#'
#' @keywords manip
#' @examples
#'
#' rmse(solubility_test, truth = solubility, estimate = prediction)
#' mae(solubility_test, truth = solubility, estimate = prediction)
#'
#' rsq(solubility_test, solubility, prediction)
#'
#' set.seed(2291)
#' solubility_test$randomized <- sample(solubility_test$prediction)
#' rsq(solubility_test, solubility, randomized)
#' rsq_trad(solubility_test, solubility, randomized)
#'
#' ccc(solubility_test, solubility, prediction)
#'
#' @export
#' @rdname rmse
rmse <- function(data, ...)
  UseMethod("rmse")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases
rmse.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    rmse_calc(data[[vars$truth]], data[[vars$estimate]])
  }

rmse_calc <- function(obs, pred)
  sqrt( mean( (obs - pred) ^ 2) )


#' @export
#' @rdname rmse
rsq <- function(data, ...)
  UseMethod("rsq")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases cor
rsq.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]

    rsq_calc(data[[vars$truth]], data[[vars$estimate]])
  }

rsq_calc <- function(obs, pred)
  cor(obs, pred)^2


#' @export
#' @rdname rmse
rsq_trad <- function(data, ...)
  UseMethod("rsq_trad")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases var
rsq_trad.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    n <- nrow(data)
    ss <- sum( (data[[vars$estimate]] - data[[vars$truth]]) ^ 2)
    1 - (ss / ((n - 1) * var(data[[vars$truth]])))
  }


#' @export
#' @rdname rmse
mae <- function(data, ...)
  UseMethod("mae")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases
mae.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    mae_calc( data[[vars$truth]], data[[vars$estimate]])
  }


mae_calc <- function(obs, pred)
  mean( abs(obs - pred) )



#' @export
#' @rdname rmse
mape <- function(data, ...)
  UseMethod("mape")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases
mape.data.frame <-
  function(data, truth, estimate, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]
    mape_calc(data[[vars$truth]], data[[vars$estimate]])
  }


mape_calc <- function(obs, pred)
  mean( abs( (obs - pred)/obs ) ) * 100


#' @export
#' @rdname rmse
ccc <- function(data, ...)
  UseMethod("ccc")

#' @rdname rmse
#' @export
#' @importFrom stats complete.cases var
ccc.data.frame <-
  function(data, truth, estimate, bias = FALSE, na.rm = TRUE, ...) {
    vars <-
      num_select(
        data = data,
        truth = !!enquo(truth),
        estimate = !!enquo(estimate),
        ...
      )
    data <- data[, c(vars$truth, vars$estimate)]
    if (na.rm)
      data <- data[complete.cases(data), ]

    m_e <- mean(data[[vars$estimate]])
    m_t <- mean(data[[vars$truth]])
    v_e <- var(data[[vars$estimate]])
    v_t <- var(data[[vars$truth]])
    cross <- scale(data[[vars$truth]], scale = FALSE) *
      scale(data[[vars$estimate]], scale = FALSE)
    cross <- mean(cross)

    if (bias) {
      n <- nrow(data)
      v_e <- v_e * (n - 1) / n
      v_t <- v_t * (n - 1) / n
    }

    2 * cross / (v_e + v_t + (m_e - m_t) ^ 2)
  }
