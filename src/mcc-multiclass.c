#include "yardstick.h"

// This implementation most directly follows the bottom of page 2 of
// https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0041882&type=printable
// You can also find it on wiki with slightly more confusing notation
// https://en.wikipedia.org/wiki/Matthews_correlation_coefficient#Multiclass_case

// `C` is an integer square confusion matrix with predictions down the rows
// and observed values in the columns.

static inline R_len_t compute_flat_index(int row, int col, R_len_t n_row);

SEXP yardstick_mcc_multiclass_impl(SEXP C) {
  const double* p_C = REAL(C);

  // `C` is square so `n_row == n_col`
  SEXP dim = PROTECT(Rf_getAttrib(C, R_DimSymbol));
  const R_len_t N = INTEGER(dim)[0];

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

  // Both the numerator and denom loop over k
  // so we extract it out to only do it once
  for(int k = 0; k < N; ++k) {
    const R_len_t loc_kk = compute_flat_index(k, k, N);

    const double C_kk = p_C[loc_kk];

    // reset to 0 for each k iteration
    C_lk_sum = 0;
    C_kl_sum = 0;

    C_gf_sum = 0;
    C_fg_sum = 0;

    // Numerator loops over l and m
    for(int l = 0; l < N; ++l) {
      const R_len_t loc_lk = compute_flat_index(l, k, N);
      const R_len_t loc_kl = compute_flat_index(k, l, N);

      const double C_lk = p_C[loc_lk];
      const double C_kl = p_C[loc_kl];

      for(int m = 0; m < N; ++m) {
        const R_len_t loc_ml = compute_flat_index(m, l, N);
        const R_len_t loc_km = compute_flat_index(k, m, N);

        const double C_ml = p_C[loc_ml];
        const double C_km = p_C[loc_km];

        // Actual increment of numerator sum
        numer += C_kk * C_ml - C_lk * C_km;
      }

      // The denominator pieces C_lk and C_kl also loop over
      // l, so we utilize that fact and go ahead and calculate their
      // terms here
      C_lk_sum += C_lk;
      C_kl_sum += C_kl;
    }

    // "sum 1 to N where f != k"
    for (R_len_t f = 0; f < N; ++f) {
      if (f == k) {
        continue;
      }

      for(R_len_t g = 0; g < N; ++g) {
        const R_len_t loc_gf = compute_flat_index(g, f, N);
        const R_len_t loc_fg = compute_flat_index(f, g, N);

        C_gf_sum += p_C[loc_gf];
        C_fg_sum += p_C[loc_fg];
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

  SEXP out = Rf_ScalarReal(res);

  UNPROTECT(1);
  return out;
}

// Compute the flat 1-D position along an array from row/col
// `row` and `col` are 0-based
static inline R_len_t compute_flat_index(int row, int col, R_len_t n_row) {
  return row + (col * n_row);
}
