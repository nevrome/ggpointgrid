#include "Rcpp.h"
using namespace Rcpp;

extern "C" int32_t futhark_entry_main_c(int l, int *a, int *b, int *c);

//' Call C function from Rcpp
//' @export
// [[Rcpp::export]]
SEXP futhark_entry_main_cpp(SEXP l, SEXP a, SEXP b, SEXP c) {
  
  // int32_t a_mod = Rcpp::as<int32_t>(a);
  // int32_t b_mod = Rcpp::as<int32_t>(b);
  
  int listLength = Rcpp::as<int>(l);
  IntegerVector a_mod(a);
  IntegerVector b_mod(b);
  IntegerVector c_mod(c);
  
  int32_t res = futhark_entry_main_c(listLength, a_mod.begin(), b_mod.begin(), c_mod.begin());
  
  return Rcpp::wrap(res);
}
