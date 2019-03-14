#include <Rcpp.h>
using namespace Rcpp;

IntegerVector order_cpp(NumericVector x, bool decreasing) {
  // sort(true) = sort decreasing
  NumericVector sorted = clone(x).sort(decreasing);
  // return the cpp order not the r order (0 index based)
  return match(sorted, x) - 1;
}

// Algorithm modified from page 866 of
// http://people.inf.elte.hu/kiss/12dwhdm/roc.pdf

// [[Rcpp::export]]
List pr_curve_cpp(IntegerVector truth, NumericVector estimate) {

  // descending order sort based on estimate order
  IntegerVector estimate_order = order_cpp(estimate, true);

  truth = truth[estimate_order];
  estimate = estimate[estimate_order];

  double fp = 0;
  double tp = 0;

  // algorithm skips repeated probabilities
  // (must re-sort because unique doesnt respect order)
  NumericVector unique_estimate = unique(estimate).sort(true);

  int n = truth.size();
  int n_positive = sum(truth == 1);
  int n_out = unique_estimate.size();

  NumericVector x_recall = NumericVector(n_out);
  NumericVector y_precision = NumericVector(n_out);

  // j is only incremented when there are no duplicates
  int j = 0;
  double estimate_i = 0;

  // Initialize with first case. Must be done ahead of time because the
  // algorithm increments AFTER the `estimate_i != estimate_previous` check
  // but we need to increment the initial value ahead of time
  double estimate_previous = unique_estimate[0];

  if(truth[0] == 1) {
    tp++;
  }
  else if (truth[0] == 2) {
    fp++;
  }

  // Start at position 1 (2 in R index world)
  // because we have dealt with the first case
  for(int i = 1; i < n; i++) {

    estimate_i = estimate[i];

    if(estimate_i != estimate_previous) {

      x_recall[j] = tp / n_positive;

      // Never undefined because of
      // the initial increment
      y_precision[j] = tp / (tp + fp);

      estimate_previous = estimate_i;
      j = j + 1;
    }

    // increment tp / fp
    if(truth[i] == 1) {
      tp++;
    }
    else if (truth[i] == 2) {
      fp++;
    }

  }

  NumericVector thresholds = unique_estimate;

  // Add end cases

  // Last row:
  // threshold = last `thresholds` value (by design, it is never used in the
  //  algorithm but already exists in `thresholds`)
  // recall = TP/P = 1 if length(P) > 0
  // precision = TP / (TP + FP) = P / N = #positives / #elements
  // ensure double division!
  x_recall[n_out - 1] = 1;
  y_precision[n_out - 1] = n_positive / (double)n;

  // First row:
  // threshold = infinity
  // recall = TP/P = 0 if length(P) > 0
  // precision = TP / (TP + FP) = undefined b/c no value was estimated as P
  //  but the general convention seems to be to put 1 here so we can
  //  start the graph in the top left corner
  thresholds.push_front(R_PosInf);
  x_recall.push_front(0);
  y_precision.push_front(1);

  return List::create(
    Named(".threshold") = thresholds,
    Named("recall")     = x_recall,
    Named("precision")  = y_precision
  );
}
