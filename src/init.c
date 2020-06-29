#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h>
#include <R_ext/Rdynload.h>

extern SEXP yardstick_mcc_multiclass_impl(SEXP);
extern SEXP yardstick_pr_curve_binary_impl(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"yardstick_mcc_multiclass_impl",    (DL_FUNC) &yardstick_mcc_multiclass_impl, 1},
  {"yardstick_pr_curve_binary_impl",   (DL_FUNC) &yardstick_pr_curve_binary_impl, 3},
  {NULL, NULL, 0}
};

void R_init_yardstick(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
