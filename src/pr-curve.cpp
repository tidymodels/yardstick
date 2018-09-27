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

  double estimate_previous = -INFINITY;

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

  for(int i = 0; i < n; i++) {

    estimate_i = estimate[i];

    // increment tp and fp as necessary
    // incrementing before the appending step is the equivalent of the
    // >= case when comparing to the threshold value (incrementing after
    // would be the > case) This is consistent with ROCR but different from
    // the paper.
    if(truth[i] == 1) {
      tp = tp + 1;
    }
    else if (truth[i] == 2) {
      fp = fp + 1;
    }

    // append to recall and precision vectors
    if(estimate_i != estimate_previous) {

      x_recall[j] = tp / n_positive;

      // as long as incrementing of tp and fp is done before this,
      // then this is never undefined
      y_precision[j] = tp / (tp + fp);

      estimate_previous = estimate_i;
      j = j + 1;
    }

  }

  NumericVector thresholds = unique_estimate;

  // Add end cases

  // threshold = infinity
  // recall = TP/P = 0 if length(P) > 0
  // precision = TP / (TP + FP) = undefined b/c no value was estimated as P
  thresholds.push_front(R_PosInf);
  x_recall.push_front(0);
  y_precision.push_front(NA_REAL);

  // This case is not needed b/c it does not add any extra info. The last
  // iteration captures the all positive case. This is also consistent with
  // ROCR
  // // threshold = -infinity
  // // recall = TP/P = 1 if length(P) > 0
  // // precision = TP / (TP + FP) = P / N = #positives / #elements
  // thresholds.push_back(R_NegInf);
  // x_recall.push_back(1);
  // // ensure double division
  // y_precision.push_back(n_positive /(double)n);

  return List::create(
    Named("threshold") = thresholds,
    Named("recall")    = x_recall,
    Named("precision") = y_precision
  );
}
