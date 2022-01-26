#include <R.h>
#include <Rinternals.h>

#include "arrange.h"

int32_t futhark_entry_main_c(int l, int *a, int *b, int *c) {
  
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  
  int32_t res;
  
  struct futhark_i32_1d *a_arr = futhark_new_i32_1d(ctx, a, l);
  struct futhark_i32_1d *b_arr = futhark_new_i32_1d(ctx, b, l);
  struct futhark_i32_1d *c_arr = futhark_new_i32_1d(ctx, c, l);
  
  futhark_entry_main(ctx, &res, a_arr, b_arr, c_arr);
  
  return res;
  
}
