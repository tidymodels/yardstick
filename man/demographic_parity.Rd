% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fair-demographic_parity.R
\name{demographic_parity}
\alias{demographic_parity}
\title{Demographic parity}
\usage{
demographic_parity(by)
}
\arguments{
\item{by}{The column identifier for the sensitive feature. This should be an
unquoted column name referring to a column in the un-preprocessed data.}
}
\value{
This function outputs a yardstick \emph{fairness metric} function. Given a
grouping variable \code{by}, \code{demographic_parity()} will return a yardstick metric
function that is associated with the data-variable grouping \code{by} and a
post-processor. The outputted function will first generate a set
of detection_prevalence metric values by group before summarizing across
groups using the post-processing function.

The outputted function only has a data frame method and is intended to
be used as part of a metric set.
}
\description{
Demographic parity is satisfied when a model's predictions have the
same predicted positive rate across groups. A value of 0 indicates parity
across groups. Note that this definition does not depend on the true
outcome; the \code{truth} argument is included in outputted metrics
for consistency.

\code{demographic_parity()} is calculated as the difference between the largest
and smallest value of \code{\link[=detection_prevalence]{detection_prevalence()}} across groups.

Demographic parity is sometimes referred to as group fairness,
disparate impact, or statistical parity.

See the "Measuring Disparity" section for details on implementation.
}
\section{Measuring Disparity}{

By default, this function takes the difference in range of detection_prevalence
\code{.estimate}s across groups. That is, the maximum pair-wise disparity between
groups is the return value of \code{demographic_parity()}'s \code{.estimate}.

For finer control of group treatment, construct a context-aware fairness
metric with the \code{\link[=new_groupwise_metric]{new_groupwise_metric()}} function by passing a custom \code{aggregate}
function:

\if{html}{\out{<div class="sourceCode">}}\preformatted{# the actual default `aggregate` is:
diff_range <- function(x, ...) \{diff(range(x$.estimate))\}

demographic_parity_2 <-
  new_groupwise_metric(
    fn = detection_prevalence,
    name = "demographic_parity_2",
    aggregate = diff_range
  )
}\if{html}{\out{</div>}}

In \code{aggregate()}, \code{x} is the \code{metric_set()} output with detection_prevalence values
for each group, and \code{...} gives additional arguments (such as a grouping
level to refer to as the "baseline") to pass to the function outputted
by \code{demographic_parity_2()} for context.
}

\examples{
library(dplyr)

data(hpc_cv)

head(hpc_cv)

# evaluate `demographic_parity()` by Resample
m_set <- metric_set(demographic_parity(Resample))

# use output like any other metric set
hpc_cv |>
  m_set(truth = obs, estimate = pred)

# can mix fairness metrics and regular metrics
m_set_2 <- metric_set(sens, demographic_parity(Resample))

hpc_cv |>
  m_set_2(truth = obs, estimate = pred)
}
\references{
Agarwal, A., Beygelzimer, A., Dudik, M., Langford, J., & Wallach, H. (2018).
"A Reductions Approach to Fair Classification." Proceedings of the 35th
International Conference on Machine Learning, in Proceedings of Machine
Learning Research. 80:60-69.

Verma, S., & Rubin, J. (2018). "Fairness definitions explained". In
Proceedings of the international workshop on software fairness (pp. 1-7).

Bird, S., Dudík, M., Edgar, R., Horn, B., Lutz, R., Milan, V., ... & Walker,
K. (2020). "Fairlearn: A toolkit for assessing and improving fairness in AI".
Microsoft, Tech. Rep. MSR-TR-2020-32.
}
\seealso{
Other fairness metrics: 
\code{\link{equal_opportunity}()},
\code{\link{equalized_odds}()}
}
\concept{fairness metrics}
