#' Costs function for poor classification
#'
#' @description
#' `classification_cost()` calculates the cost of a poor prediction based on
#' user-defined costs. The costs are multiplied by the estimated class
#' probabilities and the mean cost is returned.
#'
#' @details
#' As an example, suppose that there are three classes: `"A"`, `"B"`, and `"C"`.
#' Suppose there is a truly `"A"` observation with class probabilities `A = 0.3
#' / B = 0.3 / C = 0.4`. Suppose that, when the true result is class `"A"`, the
#' costs for each class were `A = 0 / B = 5 / C = 10`, penalizing the
#' probability of incorrectly predicting `"C"` more than predicting `"B"`. The
#' cost for this prediction would be `0.3 * 0 + 0.3 * 5 + 0.4 * 10`. This
#' calculation is done for each sample and the individual costs are averaged.
#'
#' @family class probability metrics
#' @templateVar metric_fn class_cost
#' @template return
#'
#' @inheritParams pr_auc
#'
#' @param costs A data frame with columns `"truth"`, `"estimate"`, and `"cost"`.
#'
#' `"truth"` and `"estimate"` should be character columns containing unique
#' combinations of the levels of the `truth` factor.
#'
#' `"costs"` should be a numeric column representing the cost that should
#' be applied when the `"estimate"` is predicted, but the true result is
#' `"truth"`.
#'
#' It is often the case that when `"truth" == "estimate"`, the cost is zero
#' (no penalty for correct predictions).
#'
#' If any combinations of the levels of `truth` are missing, their costs are
#' assumed to be zero.
#'
#' If `NULL`, equal costs are used, applying a cost of `0` to correct
#' predictions, and a cost of `1` to incorrect predictions.
#'
#' @author Max Kuhn
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' # ---------------------------------------------------------------------------
#' # Two class example
#' data(two_class_example)
#'
#' # Assuming `Class1` is our "event", this penalizes false positives heavily
#' costs1 <- tribble(
#'   ~truth,   ~estimate, ~cost,
#'   "Class1", "Class2",  1,
#'   "Class2", "Class1",  2
#' )
#'
#' # Assuming `Class1` is our "event", this penalizes false negatives heavily
#' costs2 <- tribble(
#'   ~truth,   ~estimate, ~cost,
#'   "Class1", "Class2",  2,
#'   "Class2", "Class1",  1
#' )
#'
#' classification_cost(two_class_example, truth, Class1, costs = costs1)
#'
#' classification_cost(two_class_example, truth, Class1, costs = costs2)
#'
#' # ---------------------------------------------------------------------------
#' # Multiclass
#' data(hpc_cv)
#'
#' # Define cost matrix from Kuhn and Johnson (2013)
#' hpc_costs <- tribble(
#'    ~estimate, ~truth, ~cost,
#'    "VF",   "VF",     0,
#'    "VF",    "F",     1,
#'    "VF",    "M",     5,
#'    "VF",    "L",    10,
#'    "F",    "VF",     1,
#'    "F",     "F",     0,
#'    "F",     "M",     5,
#'    "F",     "L",     5,
#'    "M",    "VF",     1,
#'    "M",     "F",     1,
#'    "M",     "M",     0,
#'    "M",     "L",     1,
#'    "L",    "VF",     1,
#'    "L",     "F",     1,
#'    "L",     "M",     1,
#'    "L",     "L",     0
#' )
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   classification_cost(obs, VF:L, costs = hpc_costs)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   classification_cost(obs, VF:L, costs = hpc_costs)
classification_cost <- function(data, ...) {
   UseMethod("classification_cost")
}
classification_cost <- new_prob_metric(
   fn = classification_cost,
   direction = "minimize"
)

#' @rdname classification_cost
#' @export
classification_cost.data.frame <- function(data,
                                           truth,
                                           ...,
                                           costs = NULL,
                                           na_rm = TRUE,
                                           event_level = yardstick_event_level()) {
   estimate <- dots_to_estimate(data, !!!enquos(...))

   metric_summarizer(
      metric_nm = "classification_cost",
      metric_fn = classification_cost_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!estimate,
      na_rm = na_rm,
      event_level = event_level,
      # Extra argument for classification_cost_impl()
      metric_fn_options = list(costs = costs)
   )
}

#' @rdname classification_cost
#' @export
classification_cost_vec <- function(truth,
                                    estimate,
                                    costs = NULL,
                                    na_rm = TRUE,
                                    event_level = yardstick_event_level(),
                                    ...) {
   estimator <- finalize_estimator(truth, metric_class = "classification_cost")

   classification_cost_impl <- function(truth, estimate, costs) {
      classification_cost_estimator_impl(truth, estimate, costs, estimator, event_level)
   }

   metric_vec_template(
      metric_impl = classification_cost_impl,
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      estimator = estimator,
      cls = c("factor", "numeric"),
      costs = costs
   )
}

classification_cost_estimator_impl <- function(truth,
                                               estimate,
                                               costs,
                                               estimator,
                                               event_level) {
   if (is_binary(estimator)) {
      classification_cost_binary(truth, estimate, costs, event_level)
   } else {
      classification_cost_multiclass(truth, estimate, costs)
   }
}

classification_cost_binary <- function(truth, estimate, costs, event_level) {
   if (is_event_first(event_level)) {
      level1 <- estimate
      level2 <- 1 - estimate
   } else {
      level1 <- 1 - estimate
      level2 <- estimate
   }

   estimate <- c(level1, level2)
   estimate <- matrix(estimate, ncol = 2)

   classification_cost_multiclass(truth, estimate, costs)
}

classification_cost_multiclass <- function(truth, estimate, costs) {
   levels <- levels(truth)

   costs <- validate_costs(costs, levels)
   costs <- pivot_costs(costs, levels)
   costs <- recycle_costs(costs, truth)

   costs <- costs[levels]
   costs <- as.matrix(costs)

   out <- estimate * costs
   out <- rowSums(out)
   out <- mean(out)

   out
}

validate_costs <- function(costs, levels) {
   if (is.null(costs)) {
      costs <- generate_equal_cost_grid(levels)
      return(costs)
   }

   if (!is.data.frame(costs)) {
      abort("`costs` must be `NULL` or a data.frame.")
   }

   columns <- names(costs)
   if (length(columns) != 3L) {
      abort("`costs` must be a data.frame with 3 columns.")
   }

   ok <- identical(sort(columns), sort(c("truth", "estimate", "cost")))
   if (!ok) {
      abort("`costs` must have columns: 'truth', 'estimate', and 'cost'.")
   }

   if (is.factor(costs$truth)) {
      costs$truth <- as.character(costs$truth)
   }
   if (!is.character(costs$truth)) {
      abort("`costs$truth` must be a character or factor column.")
   }

   if (is.factor(costs$estimate)) {
      costs$estimate <- as.character(costs$estimate)
   }
   if (!is.character(costs$estimate)) {
      abort("`costs$estimate` must be a character or factor column.")
   }

   if (!is.numeric(costs$cost)) {
      abort("`costs$cost` must be a numeric column.")
   }

   ok <- all(costs$truth %in% levels)
   if (rlang::is_false(ok)) {
      levels <- quote_and_collapse(levels)
      abort(paste0("`costs$truth` can only contain ", levels, "."))
   }

   ok <- all(costs$estimate %in% levels)
   if (rlang::is_false(ok)) {
      levels <- quote_and_collapse(levels)
      abort(paste0("`costs$estimate` can only contain ", levels, "."))
   }

   pairs <- costs[c("truth", "estimate")]
   cost <- costs$cost

   not_ok <- vctrs::vec_duplicate_any(pairs)
   if (not_ok) {
      abort("`costs` cannot have duplicate 'truth' / 'estimate' combinations.")
   }

   out <- generate_all_level_combinations(levels)

   # Detect user specified cost locations
   locs <- vctrs::vec_match(pairs, out)

   # Default to no cost
   out$cost <- 0

   # Update to user specified cost
   out$cost[locs] <- cost

   out
}

# - 0 cost for correct predictions
# - 1 cost for incorrect predictions
generate_equal_cost_grid <- function(levels) {
   costs <- generate_all_level_combinations(levels)
   costs$cost <- ifelse(costs$truth == costs$estimate, yes = 0, no = 1)
   costs
}

generate_all_level_combinations <- function(levels) {
   # `expand.grid()` expands first column fastest,
   # but we want first column slowest so we reverse the columns
   grid <- expand.grid(estimate = levels, truth = levels)
   grid <- dplyr::as_tibble(grid)
   grid <- grid[c("truth", "estimate")]
   grid
}

pivot_costs <- function(costs, levels) {
   # Must be a data frame, not a tibble, for `reshape()` to work
   costs <- as.data.frame(costs)

   # tidyr::pivot_wider(costs, truth, names_from = estimate, values_from = cost)
   costs <- stats::reshape(
      costs,
      v.names = "cost",
      idvar = "truth",
      timevar = "estimate",
      direction = "wide",
      sep = "."
   )

   # Ensure column ordering matches `truth` level ordering
   columns <- paste0("cost.", levels)
   costs <- costs[c("truth", columns)]

   names(costs) <- c("truth", levels)

   costs <- dplyr::as_tibble(costs)

   costs
}

recycle_costs <- function(costs, truth) {
   levels <- levels(truth)

   # Expand `costs` to equal size of `truth`, matching the `truth` values
   needles <- truth
   haystack <- factor(costs$truth, levels = levels)
   locs <- vctrs::vec_match(needles, haystack)

   costs <- vctrs::vec_slice(costs, locs)

   costs
}
