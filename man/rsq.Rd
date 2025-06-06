% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/num-rsq.R
\name{rsq}
\alias{rsq}
\alias{rsq.data.frame}
\alias{rsq_vec}
\title{R squared}
\usage{
rsq(data, ...)

\method{rsq}{data.frame}(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

rsq_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
}
\arguments{
\item{data}{A \code{data.frame} containing the columns specified by the \code{truth}
and \code{estimate} arguments.}

\item{...}{Not currently used.}

\item{truth}{The column identifier for the true results
(that is \code{numeric}). This should be an unquoted column name although
this argument is passed by expression and supports
\link[rlang:topic-inject]{quasiquotation} (you can unquote column
names). For \verb{_vec()} functions, a \code{numeric} vector.}

\item{estimate}{The column identifier for the predicted
results (that is also \code{numeric}). As with \code{truth} this can be
specified different ways but the primary method is to use an
unquoted variable name. For \verb{_vec()} functions, a \code{numeric} vector.}

\item{na_rm}{A \code{logical} value indicating whether \code{NA}
values should be stripped before the computation proceeds.}

\item{case_weights}{The optional column identifier for case weights. This
should be an unquoted column name that evaluates to a numeric column in
\code{data}. For \verb{_vec()} functions, a numeric vector,
\code{\link[hardhat:importance_weights]{hardhat::importance_weights()}}, or \code{\link[hardhat:frequency_weights]{hardhat::frequency_weights()}}.}
}
\value{
A \code{tibble} with columns \code{.metric}, \code{.estimator},
and \code{.estimate} and 1 row of values.

For grouped data frames, the number of rows returned will be the same as
the number of groups.

For \code{rsq_vec()}, a single \code{numeric} value (or \code{NA}).
}
\description{
Calculate the coefficient of determination using correlation. For the
traditional measure of R squared, see \code{\link[=rsq_trad]{rsq_trad()}}.
}
\details{
The two estimates for the
coefficient of determination, \code{\link[=rsq]{rsq()}} and \code{\link[=rsq_trad]{rsq_trad()}}, differ by
their formula. The former guarantees a value on (0, 1) while the
latter can generate inaccurate values when the model is
non-informative (see the examples). Both are measures of
consistency/correlation and not of accuracy.

\code{rsq()} is simply the squared correlation between \code{truth} and \code{estimate}.

Because \code{rsq()} internally computes a correlation, if either \code{truth} or
\code{estimate} are constant it can result in a divide by zero error. In these
cases, a warning is thrown and \code{NA} is returned. This can occur when a model
predicts a single value for all samples. For example, a regularized model
that eliminates all predictors except for the intercept would do this.
Another example would be a CART model that contains no splits.
}
\examples{
# Supply truth and predictions as bare column names
rsq(solubility_test, solubility, prediction)

library(dplyr)

set.seed(1234)
size <- 100
times <- 10

# create 10 resamples
solubility_resampled <- bind_rows(
  replicate(
    n = times,
    expr = sample_n(solubility_test, size, replace = TRUE),
    simplify = FALSE
  ),
  .id = "resample"
)

# Compute the metric by group
metric_results <- solubility_resampled |>
  group_by(resample) |>
  rsq(solubility, prediction)

metric_results

# Resampled mean estimate
metric_results |>
  summarise(avg_estimate = mean(.estimate))
# With uninformitive data, the traditional version of R^2 can return
# negative values.
set.seed(2291)
solubility_test$randomized <- sample(solubility_test$prediction)
rsq(solubility_test, solubility, randomized)
rsq_trad(solubility_test, solubility, randomized)

# A constant `truth` or `estimate` vector results in a warning from
# a divide by zero error in the correlation calculation.
# `NA` will be returned in these cases.
truth <- c(1, 2)
estimate <- c(1, 1)
rsq_vec(truth, estimate)
}
\references{
Kvalseth. Cautionary note about \eqn{R^2}.
American Statistician (1985) vol. 39 (4) pp. 279-285.
}
\seealso{
Other numeric metrics: 
\code{\link{ccc}()},
\code{\link{huber_loss}()},
\code{\link{huber_loss_pseudo}()},
\code{\link{iic}()},
\code{\link{mae}()},
\code{\link{mape}()},
\code{\link{mase}()},
\code{\link{mpe}()},
\code{\link{msd}()},
\code{\link{poisson_log_loss}()},
\code{\link{rmse}()},
\code{\link{rpd}()},
\code{\link{rpiq}()},
\code{\link{rsq_trad}()},
\code{\link{smape}()}

Other consistency metrics: 
\code{\link{ccc}()},
\code{\link{rpd}()},
\code{\link{rpiq}()},
\code{\link{rsq_trad}()}
}
\author{
Max Kuhn
}
\concept{consistency metrics}
\concept{numeric metrics}
