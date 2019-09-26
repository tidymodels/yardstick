#include <Rcpp.h>
using namespace Rcpp;

// Algorithm modified from page 866 of
// http://people.inf.elte.hu/kiss/12dwhdm/roc.pdf

// Details:
// - Sort unique class probabilities in descending order.
//   These are your thresholds (Done at the R level).
//
// - Initialize the "previous" threshold with the first threshold value.
//
// - Iterate through all of the class probability values (including duplicates)
//   For each:
//
//    - Check if the current class prob value is the same as the previous one.
//
//    - If different, we have a unique threshold so append to the
//      recall and precision. This actually updates the _previous_ threshold's
//      recall and precision value. It does it this way to correct capture any
//      duplicate probability values.
//
//    - After the check, bump the tp or fp counter, depending on whether or not
//      this iteration was a "success"
//
//    - By design, since we always update the previous threshold when we hit a
//      new threshold, the last threshold value never gets updated. So we
//      manually add that value afterwards outside the loop.
//
// Notes:
// - We always prepend recall = 0, precision = 1 at the very end
//   to start the curve in the right spot. This is _necessary_ for PR AUC
//   values to be computed right.
//
// - Since we initialize the "previous" value with the first threshold, we never
//   run the risk of computing an undefined precision (tp=0, fp=0). The first
//   iteration always skips straight to the incrementing of tp and fp.

// [[Rcpp::export]]
List pr_curve_cpp(IntegerVector truth,
                  NumericVector estimate,
                  NumericVector thresholds) {

  double fp = 0;
  double tp = 0;

  int n = truth.size();
  int n_positive = sum(truth == 1);
  int n_out = thresholds.size();

  if (n_positive == 0) {
    Rcpp::warning(
      "There are `0` event cases in `truth`, results will be meaningless."
    );
  }

  NumericVector x_recall = NumericVector(n_out, NA_REAL);
  NumericVector y_precision = NumericVector(n_out, NA_REAL);

  // j is only incremented when there are no duplicates
  int j = 0;

  // Initialize with first case.
  double threshold_previous = thresholds[0];

  double threshold_i;

  for(int i = 0; i < n; i++) {

    threshold_i = estimate[i];

    if(threshold_i != threshold_previous) {

      x_recall[j] = tp / n_positive;

      y_precision[j] = tp / (tp + fp);

      threshold_previous = threshold_i;

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

  // Add end cases

  // Last row:
  // threshold = taken care of already. Exists as last value of `thresholds`.
  // recall    = TP/P = 1 if length(P) > 0
  // precision = TP / (TP + FP) = P / N = #positives / #elements
  // ensure double division!
  if (n_positive > 0) {
    x_recall[n_out - 1] = 1;
  }

  y_precision[n_out - 1] = n_positive / (double)n;

  // First row:
  // threshold = infinity
  // recall    = TP/P = 0 if length(P) > 0
  // precision = TP / (TP + FP) = undefined b/c we haven't seen any values yet
  //  but we need to put 1 here so we can start the graph in the top left corner
  //  and compute PR AUC correctly
  thresholds.push_front(R_PosInf);
  x_recall.push_front(0);
  y_precision.push_front(1);

  return List::create(
    Named(".threshold") = thresholds,
    Named("recall")     = x_recall,
    Named("precision")  = y_precision
  );
}
