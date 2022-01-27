#include "Rcpp.h"
using namespace Rcpp;

extern "C" void futhark_entry_main_c(
    int l,
    int *in_a, int *in_b, int *in_c,
    int *out_a, int *out_b, int *out_c
  );

//' Call C function from Rcpp
//' @export
// [[Rcpp::export]]
SEXP futhark_entry_main_cpp(SEXP l, SEXP a, SEXP b, SEXP c) {
  
  // int32_t a_mod = Rcpp::as<int32_t>(a);
  // int32_t b_mod = Rcpp::as<int32_t>(b);
  
  int listLength = Rcpp::as<int>(l);
  IntegerVector in_a_iv(a);
  IntegerVector in_b_iv(b);
  IntegerVector in_c_iv(c);
  IntegerVector out_a_iv = IntegerVector(l);
  IntegerVector out_b_iv = IntegerVector(l);
  IntegerVector out_c_iv = IntegerVector(l);
  
  futhark_entry_main_c(
    listLength,
    in_a_iv.begin(), in_b_iv.begin(), in_c_iv.begin(),
    out_a_iv.begin(), out_b_iv.begin(), out_c_iv.begin()
  );
  
  return Rcpp::wrap(List::create(out_a_iv, out_b_iv, out_c_iv));
}
