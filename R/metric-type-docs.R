# Internal helper to generate metric list for documentation
metric_type_list <- function(type) {
  yardns <- asNamespace("yardstick")
  fns <- lapply(names(yardns), get, envir = yardns)
  names(fns) <- names(yardns)

  type_class <- paste0(type, "_metric")
  metrics <- fns[vapply(
    fns,
    inherits,
    what = type_class,
    FUN.VALUE = logical(1)
  )]
  metrics <- metrics[sort(names(metrics))]

  items <- vapply(
    names(metrics),
    function(nm) {
      fn <- metrics[[nm]]
      dir <- attr(fn, "direction")
      rng <- attr(fn, "range")
      rng_str <- paste0("\\[", rng[1], ", ", rng[2], "\\]")
      paste0(
        "\\item{\\code{\\link[=",
        nm,
        "]{",
        nm,
        "()}}}{",
        "Direction: ",
        dir,
        ". Range: ",
        rng_str,
        "}"
      )
    },
    character(1)
  )

  paste0("\\describe{\n", paste(items, collapse = "\n"), "\n}")
}

# ------------------------------------------------------------------------------

#' Class metrics
#'
#' @description
#' Class metrics evaluate hard classification predictions where both `truth`
#' and `estimate` are factors. These metrics compare predicted classes directly
#' against the true classes.
#'
#' @section Input requirements:
#' - `truth`: factor
#' - `estimate`: factor
#'
#' @section Available metrics:
#' `r metric_type_list("class")`
#'
#' @examples
#' data("two_class_example")
#'
#' head(two_class_example)
#'
#' accuracy(two_class_example, truth, predicted)
#'
#' @seealso
#' [prob-metrics] for class probability metrics
#'
#' [ordered-prob-metrics] for ordered probability metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name class-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Class probability metrics
#'
#' @description
#' Class probability metrics evaluate soft classification predictions where
#' `truth` is a factor and `estimate` consists of class probability columns.
#' These metrics assess how well predicted probabilities match the true class
#' membership.
#'
#' @section Input requirements:
#' - `truth`: factor
#' - `estimate` / `...`: numeric columns containing class probabilities
#'
#' @section Available metrics:
#' `r metric_type_list("prob")`
#'
#' @examples
#' data("two_class_example")
#'
#' head(two_class_example)
#'
#' roc_auc(two_class_example, truth, Class1)
#'
#' @seealso
#' [class-metrics] for hard classification metrics
#'
#' [ordered-prob-metrics] for ordered probability metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name prob-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Ordered probability metrics
#'
#' @description
#' Ordered probability metrics evaluate predictions for ordered factor outcomes
#' where the class probabilities should respect the natural ordering of the
#' levels.
#'
#' @section Input requirements:
#' - `truth`: ordered factor
#' - `estimate` / `...`: numeric columns containing class probabilities
#'
#' @section Available metrics:
#' `r metric_type_list("ordered_prob")`
#'
#' @examples
#' # Example with an ordered factor
#' set.seed(1)
#' df <- data.frame(
#'   truth = ordered(sample(1:3, 20, replace = TRUE)),
#'   prob_1 = runif(20),
#'   prob_2 = runif(20),
#'   prob_3 = runif(20)
#' )
#' # Normalize probabilities
#' df[2:4] <- df[2:4] / rowSums(df[2:4])
#'
#' ranked_prob_score(df, truth, prob_1:prob_3)
#'
#' @seealso
#' [class-metrics] for hard classification metrics
#'
#' [prob-metrics] for class probability metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name ordered-prob-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Numeric metrics
#'
#' @description
#' Numeric metrics evaluate regression predictions where both `truth` and
#' `estimate` are numeric. These metrics measure how close predicted values
#' are to the true values.
#'
#' @section Input requirements:
#' - `truth`: numeric
#' - `estimate`: numeric
#'
#' @section Available metrics:
#' `r metric_type_list("numeric")`
#'
#' @examples
#' data("solubility_test")
#'
#' head(solubility_test)
#'
#' rmse(solubility_test, solubility, prediction)
#'
#' @seealso
#' [quantile-metrics] for quantile prediction metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name numeric-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Dynamic survival metrics
#'
#' @description
#' Dynamic survival metrics evaluate time-dependent survival predictions,
#' producing one metric value per evaluation time point. These metrics assess
#' predicted survival probabilities at specific time points.
#'
#' @section Input requirements:
#' - `truth`: a [survival::Surv()] object
#' - `...`: list column of data frames containing `.eval_time`,
#'   `.pred_survival`, and `.weight_censored` columns
#'
#' @section Available metrics:
#' `r metric_type_list("dynamic_survival")`
#'
#' @examplesIf rlang::is_installed("tidyr")
#' library(dplyr)
#' data("lung_surv")
#'
#' head(lung_surv)
#'
#' lung_surv |>
#'   brier_survival(truth = surv_obj, .pred)
#'
#' @seealso
#' [integrated-survival-metrics] for integrated survival metrics
#'
#' [static-survival-metrics] for static survival metrics
#'
#' [linear-pred-survival-metrics] for linear predictor survival metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name dynamic-survival-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Integrated survival metrics
#'
#' @description
#' Integrated survival metrics summarize model performance across multiple
#' evaluation time points into a single value by integrating dynamic survival
#' metrics over time.
#'
#' @section Input requirements:
#' - `truth`: a [survival::Surv()] object
#' - `...`: list column of data frames containing `.eval_time`,
#'   `.pred_survival`, and `.weight_censored` columns
#'
#' @section Available metrics:
#' `r metric_type_list("integrated_survival")`
#'
#' @examplesIf rlang::is_installed("tidyr")
#' library(dplyr)
#' data("lung_surv")
#'
#' head(lung_surv)
#'
#' lung_surv |>
#'   brier_survival_integrated(truth = surv_obj, .pred)
#'
#' @seealso
#' [dynamic-survival-metrics] for time-dependent survival metrics
#'
#' [static-survival-metrics] for static survival metrics
#'
#' [linear-pred-survival-metrics] for linear predictor survival metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name integrated-survival-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Static survival metrics
#'
#' @description
#' Static survival metrics evaluate survival predictions that do not depend on
#' a specific evaluation time point. These metrics typically compare predicted
#' risk scores or survival times against observed outcomes.
#'
#' @section Input requirements:
#' - `truth`: a [survival::Surv()] object
#' - `estimate`: numeric (predicted time or risk score)
#'
#' @section Available metrics:
#' `r metric_type_list("static_survival")`
#'
#' @examples
#' data("lung_surv")
#'
#' head(lung_surv)
#'
#' concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time)
#'
#' @seealso
#' [dynamic-survival-metrics] for time-dependent survival metrics
#'
#' [integrated-survival-metrics] for integrated survival metrics
#'
#' [linear-pred-survival-metrics] for linear predictor survival metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name static-survival-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Linear predictor survival metrics
#'
#' @description
#' Linear predictor survival metrics evaluate survival model predictions based
#' on linear predictors (log-hazard or log-risk scores).
#'
#' @section Input requirements:
#' - `truth`: a [survival::Surv()] object
#' - `estimate`: numeric (linear predictor values)
#'
#' @section Available metrics:
#' `r metric_type_list("linear_pred_survival")`
#'
#' @examplesIf rlang::is_installed("tidyr")
#' data("lung_surv")
#'
#' head(lung_surv)
#'
#' lung_surv |>
#'   royston_survival(truth = surv_obj, estimate = .pred_linear_pred)
#'
#' @seealso
#' [dynamic-survival-metrics] for time-dependent survival metrics
#'
#' [integrated-survival-metrics] for integrated survival metrics
#'
#' [static-survival-metrics] for static survival metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name linear-pred-survival-metrics
#' @keywords internal
NULL

# ------------------------------------------------------------------------------

#' Quantile metrics
#'
#' @description
#' Quantile metrics evaluate predictions that consist of predicted quantiles
#' rather than point predictions. These metrics assess the accuracy and
#' calibration of distributional forecasts.
#'
#' @section Input requirements:
#' - `truth`: numeric
#' - `estimate`: [hardhat::quantile_pred] object
#'
#' @section Available metrics:
#' `r metric_type_list("quantile")`
#'
#' @examplesIf rlang::is_installed("hardhat")
#' library(hardhat)
#'
#' df <- data.frame(
#'   preds = quantile_pred(rbind(1:4, 8:11), c(0.2, 0.4, 0.6, 0.8)),
#'   truth = c(3.3, 7.1)
#' )
#'
#' df
#'
#' weighted_interval_score(df, truth, preds)
#'
#' @seealso
#' [numeric-metrics] for point prediction metrics
#'
#' `vignette("metric-types")` for an overview of all metric types
#'
#' @name quantile-metrics
#' @keywords internal
NULL
