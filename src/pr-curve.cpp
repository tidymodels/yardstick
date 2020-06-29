#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

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
// - We always prepend recall = 0, precision = 1 to start the curve.
//   This is _necessary_ for PR AUC values to be computed right. We have
//   already prepended `Inf` at the beginning of `thresholds` on the R side.
//
//   At the start of the curve:
//   threshold = infinity
//   recall    = TP/P = 0 if length(P) > 0
//   precision = TP / (TP + FP) = undefined b/c we haven't seen any values yet
//               but we need to put 1 here so we can start the graph in the top
//               left corner and compute PR AUC correctly
//
// - Since we initialize the "previous" value with the first threshold, we never
//   run the risk of computing an undefined precision (tp=0, fp=0). The first
//   iteration always skips straight to the incrementing of tp and fp.

// [[Rcpp::export]]
SEXP yardstick_pr_curve_binary_impl(SEXP truth, SEXP estimate, SEXP thresholds) {
  const R_len_t n = Rf_length(truth);
  const R_len_t n_out = Rf_length(thresholds);

  const int* p_truth = INTEGER(truth);
  const double* p_estimate = REAL(estimate);
  const double* p_thresholds = REAL(thresholds);

  int n_positive = 0;
  for(R_len_t i = 0; i < n; ++i) {
    if (p_truth[i] == 1) {
      ++n_positive;
    }
  }

  if (n_positive == 0) {
    Rf_warningcall(
      R_NilValue,
      "There are `0` event cases in `truth`, results will be meaningless."
    );
  }

  SEXP recall = PROTECT(Rf_allocVector(REALSXP, n_out));
  double* p_recall = REAL(recall);

  SEXP precision = PROTECT(Rf_allocVector(REALSXP, n_out));
  double* p_precision =  REAL(precision);

  // First recall is always `0`.
  // First precision is always `1`.
  p_recall[0] = 0;
  p_precision[0] = 1;

  // `j` is only incremented when there are no duplicates.
  // Start at `1` because 0-th row is always the same.
  R_len_t j = 1;

  // Initialize with first case
  double threshold_previous = p_thresholds[j];

  double fp = 0;
  double tp = 0;

  for(int i = 0; i < n; ++i) {
    const double threshold_current = p_estimate[i];

    if(threshold_current != threshold_previous) {
      p_recall[j] = tp / n_positive;
      p_precision[j] = tp / (tp + fp);
      ++j;

      threshold_previous = threshold_current;
    }

    const int elt_truth = p_truth[i];

    // Increment tp or fp
    if(elt_truth == 1) {
      ++tp;
    } else if (elt_truth == 2) {
      ++fp;
    }
  }

  // Last row:
  // threshold = taken care of already. Exists as last value of `thresholds`.
  // recall    = TP / P = 1 if length(P) > 0
  // precision = TP / (TP + FP) = P / N = #positives / #elements
  // Ensure double division!
  if (n > 0) {
    p_recall[n_out - 1] = (n_positive > 0) ? 1 : R_NaN;
    p_precision[n_out - 1] = n_positive / (double) n;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(out, 0, thresholds);
  SET_VECTOR_ELT(out, 1, recall);
  SET_VECTOR_ELT(out, 2, precision);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SEXP* p_names = STRING_PTR(names);

  p_names[0] = Rf_mkCharCE(".threshold", CE_UTF8);
  p_names[1] = Rf_mkCharCE("recall", CE_UTF8);
  p_names[2] = Rf_mkCharCE("precision", CE_UTF8);

  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(4);
  return out;
}
