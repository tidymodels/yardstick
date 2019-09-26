#include <Rcpp.h>
using namespace Rcpp;

// This implementation most directly follows the bottom of page 2 of
// https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0041882&type=printable
// You can also find it on wiki with slightly more confusing notation
// https://en.wikipedia.org/wiki/Matthews_correlation_coefficient#Multiclass_case

// x is a confusion matrix with predictions down the rows
// and observed values in the columns.

// [[Rcpp::export]]
double mcc_multiclass_cpp(NumericMatrix x) {

  // Change to C to match the algo terminology
  NumericMatrix C = x;

  int N = C.ncol();

  // Required for the f sum as this is an easy way to avoid
  // the k-th element in that loop. Subtract 1 because we want
  // f to be a Cpp indexer
  IntegerVector N_seq = seq_len(N) - 1;

  // The pieces of the MCC calculation are
  // separated into numerator and denom,
  // and the 2 denom pieces are further separated
  // into their individual C_**_sum pieces
  double numer = 0;
  double denom1 = 0;
  double denom2 = 0;

  double C_lk_sum = 0;
  double C_kl_sum = 0;

  double C_gf_sum = 0;
  double C_fg_sum = 0;

  // Both the numerator and denom loop over k so we extract it out to only
  // do it once
  for(int k = 0; k < N; k++) {

    // reset to 0 for each k iteration
    C_lk_sum = 0;
    C_kl_sum = 0;

    C_gf_sum = 0;
    C_fg_sum = 0;

    // Numerator loops over l and m
    for(int l = 0; l < N; l++) {

      for(int m = 0; m < N; m++) {
        // Actual increment of numerator sum
        numer = numer + C(k, k) * C(m, l) - C(l, k) * C(k, m);
      }

      // The denominator pieces C_lk and C_kl also loop over
      // l, so we utilize that fact and go ahead and calculate their
      // terms here
      C_lk_sum = C_lk_sum + C(l, k);
      C_kl_sum = C_kl_sum + C(k, l);
    }

    // "sum 1 to N where f != k"
    IntegerVector N_no_k = N_seq[N_seq != k];

    // Loop over the iterator here as it is not the same
    // as a simple sequence from 1:N (the k-th element is missing)
    for(IntegerVector::iterator f_it = N_no_k.begin();
        f_it != N_no_k.end();
        ++f_it) {

      for(int g = 0; g < N; g++) {
        C_gf_sum = C_gf_sum + C(g, *f_it);
        C_fg_sum = C_fg_sum + C(*f_it, g);
      }
    }

    // Actual increment of denom pieces
    denom1 = denom1 + C_lk_sum * C_gf_sum;
    denom2 = denom2 + C_kl_sum * C_fg_sum;
  }

  // Square root both denom pieces at the end
  double denom1_sqrt = sqrt(denom1);
  double denom2_sqrt = sqrt(denom2);

  // Final calculation of MCC
  double res = numer / (denom1_sqrt * denom2_sqrt);

  return res;
}
