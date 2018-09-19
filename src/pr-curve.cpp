#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List pr_curve_cpp(NumericVector truth, NumericVector estimate) {

  double fp = 0;
  double tp = 0;

  double estimate_previous = -INFINITY;

  // algorithm skips repeated probabilities
  NumericVector unique_estimate = unique(estimate);
  int n_positive = sum(truth == 1L);
  int n_out = unique_estimate.size();

  NumericVector x_recall = NumericVector(n_out);
  NumericVector y_precision = NumericVector(n_out);

  // j is only incremented when there are no duplicates
  int j = 0;
  double estimate_i = 0;

  for(int i = 0; i <= truth.size(); i++) {

    estimate_i = estimate[i];

    // append to recall and precision vectors
    if(estimate_i != estimate_previous) {

      x_recall[j] = tp / n_positive;

      if(tp == 0) {
        y_precision[j] = 1;
      } else {
        y_precision[j] = tp / (tp + fp);
      }

      estimate_previous = estimate_i;
      j = j + 1;
    }

    // increment tp and fp if necessary
    if(truth[i] == 1) {
      tp = tp + 1;
    }
    else if (truth[i] == 2) {
      fp = fp + 1;
    }
  }

  // Add end cases
  // (min value of precision is 0.5)
  x_recall.push_front(0);
  x_recall.push_back(1);

  y_precision.push_front(1);
  y_precision.push_back(0.5);

  NumericVector thresholds = estimate;
  thresholds.push_front(R_PosInf);
  thresholds.push_back(R_NegInf);

  return List::create(Named("threshold") = thresholds,
                      Named("recall")    = x_recall,
                      Named("precision") = y_precision);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
data("two_class_example")
pr_curve_cpp(two_class_example$truth, two_class_example$Class1)
*/
