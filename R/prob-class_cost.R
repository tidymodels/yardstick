#' Costs function for poor classification
#'
#' `class_cost()` calculates the cost of a poor prediction based on user-defined
#' costs. The costs are combined with the estimated class probabilities and
#' the mean cost is returned.
#'
#' As an example, suppose that there are three classes: `"A"`, `"B"`, and `"C"`.
#' Suppose there is a truly `"A"` data point with class probabilities
#' `A = 0.3`, `B = 0.3`, and `C = 0.4`. A cost matrix is created for all
#' combinations of the observed and predicted classes. Suppose that, when the
#' true result is class `"A"`, the costs for each class were `A = 0`, `B = 5`,
#' and `C = 10`, the cost for this prediction would be `0 + 0.3 * 5 + 0.4 * 10`.
#' This calculation is done for each sample and the individual costs are
#' averaged.
#'
#' @family class probability metrics
#' @templateVar metric_fn class_cost
#' @template return

#' @inheritParams pr_auc
#'
#' @param costs A data frame with columns `truth`, `.pred_class`, and `costs`.
#' The first two columns contain the levels of the outcome factor. The `costs`
#' column is a numeric value for the cost of the result. It is not required but
#' typical that the costs when `truth == .pred_class` are zero. If any
#' combinations are missing, their costs are assumed to be zero. If
#' `costs = NULL`, equal costs across columns are used.
#'
#' @author Max Kuhn
#'
#' @examples
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' # Define costs matrix from Kuhn and Johnson (2013):
#'
#' hpc_costs <-
#'   dplyr::tribble(
#'     ~.pred_class, ~truth, ~cost,
#'     "VF",   "VF",     0,
#'     "VF",    "F",     1,
#'     "VF",    "M",     5,
#'     "VF",    "L",    10,
#'     "F",    "VF",     1,
#'     "F",     "F",     0,
#'     "F",     "M",     5,
#'     "F",     "L",     5,
#'     "M",    "VF",     1,
#'     "M",     "F",     1,
#'     "M",     "M",     0,
#'     "M",     "L",     1,
#'     "L",    "VF",     1,
#'     "L",     "F",     1,
#'     "L",     "M",     1,
#'     "L",     "L",     0
#'   )
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   class_cost(obs, VF:L, costs = hpc_costs)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   class_cost(obs, VF:L, costs = hpc_costs)
#'
#' # ------------------------------------------------------------------------------
#' # To include in a metric set, a wrapper must be used. Based on the advice in
#' # ?metric_set:
#'
#' hpc_cost_metric <- function(data, truth, estimate, na_rm = TRUE, ...) {
#'   class_cost(
#'     data = data,
#'     truth = {{truth}},
#'     estimate = {{estimate}},
#'
#'     # set specific costs
#'     costs = hpc_costs,
#'     na_rm = na_rm,
#'     ...
#'   )
#' }
#'
#' hpc_cost_metric <- new_prob_metric(hpc_cost_metric, "minimize")
#'
#' # On its own:
#' hpc_cv %>% hpc_cost_metric(obs, VF:L)
#'
#' # In a metric set:
#' two_metrics <- metric_set(roc_auc, class_cost)
#' hpc_cv %>% two_metrics(obs, VF:L)
#'
#' @export
class_cost <- function(data, ...) {
   UseMethod("class_cost")
}

class(class_cost) <- c("prob_metric", "function")
attr(class_cost, "direction") <- "minimize"

#' @export
#' @rdname class_cost
#' @importFrom rlang quo
class_cost.data.frame <- function(data, truth, ..., estimator = NULL, na_rm = TRUE, costs = NULL,
                                  event_level = yardstick_event_level()) {

   estimate <- dots_to_estimate(data, !!!enquos(...))

   metric_summarizer(
      metric_nm = "class_cost",
      metric_fn = class_cost_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!estimate,
      estimator = estimator,
      na_rm = na_rm,
      event_level = event_level,
      # Extra argument for class_cost_impl()
      metric_fn_options = list(costs = costs)
   )

}

#' @rdname class_cost
#' @export
class_cost_vec <- function(truth, estimate, estimator, na_rm = TRUE, costs = NULL, ...) {

   estimator <- finalize_estimator(truth, metric_class = "class_cost")

   # estimate here is a matrix of class prob columns
   class_cost_impl <- function(truth, estimate, costs) {
      class_cost_estimator_impl(truth, estimate, estimator, event_level, costs)
   }

   metric_vec_template(
      metric_impl = class_cost_impl,
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      estimator = estimator,
      cls = c("factor", "numeric"),
      ...,
      costs = costs
   )
}

finalize_estimator_internal.class_cost <- function(metric_dispatcher, x, estimator) {
   if (is_multiclass(x)) {
      "multiclass"
   }
   else {
      "binary"
   }
}

check_costs <- function(x, lvls) {
   num_lvl <- length(lvls)
   chr_lvls <- paste0("'", lvls, "'", collapse = ", ")
   no_costs <- tidyr::crossing(truth = lvls, .pred_class = lvls, cost = 0)

   if (is.null(x)) {
      # default to equal costs for wrong answer
      no_costs <- dplyr::mutate(no_costs,cost = ifelse(truth != .pred_class, 1, 0))
      return(no_costs)
   }
   msg <- paste("The 'cost' argument should be a data frame with columns",
                "'truth', '.pred_class', and 'cost'.")

   if (!isTRUE(all.equal(sort(names(x)), c(".pred_class", "cost", "truth")))) {
      rlang::abort(msg)
   }

   x <- dplyr::mutate_if(x, is.factor, as.character)
   if (any(!(x$truth %in% lvls))) {
      msg <- paste("The 'truth' column of the 'cost' argument should only",
                    "have values:", chr_lvls)
      rlang::abort(msg)
   }
   if (any(!(x$.pred_class %in% lvls))) {
      msg <- paste("The '.pred_class' column of the 'cost' argument should only",
                   "have values:", chr_lvls)
      rlang::abort(msg)
   }

   if (!is.numeric(x$cost)) {
      rlang::abort("The `cost` column should be numeric.")
   }

   missing_combos <- dplyr::anti_join(no_costs[, -3], x, by = c("truth", ".pred_class"))
   if (nrow(missing_combos) > 0) {
      missing_combos$cost <- 0.0
      x <- dplyr::bind_rows(x, missing_combos)
   }

   x
}


class_cost_estimator_impl <-
   function(truth, estimate, estimator, event_level = NULL, costs = NULL) {
      lvls <- levels(truth)
      costs <- check_costs(costs, lvls)
      # When the `tune` package is used, the estimates will have the prefix
      # ".pred_" so this will fix the merge.
      colnames(estimate) <- lvls

      estimate <- dplyr::as_tibble(estimate)
      estimate$truth <- truth
      res <-
         dplyr::mutate(estimate, .row = dplyr::row_number()) %>%
         tidyr::pivot_longer(cols = c(-truth, -.row),
                             names_to = ".pred_class",
                             values_to = "probability") %>%
         dplyr::left_join(costs, by = c("truth", ".pred_class")) %>%
         dplyr::mutate(expected_cost = probability * cost) %>%
         dplyr::group_by(.row) %>%
         dplyr::summarize(cost = sum(expected_cost, na.rm = TRUE), .groups = "drop") %>%
         dplyr::ungroup()
      mean(res$cost, na.rm = TRUE)
   }

