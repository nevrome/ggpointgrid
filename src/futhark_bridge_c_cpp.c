#include <R.h>
#include <Rinternals.h>

#include "arrange.h"

void futhark_entry_main_c(int l, int *in_a, int *in_b, int *in_c, int *out_a, int *out_b, int *out_c) {
  
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  struct futhark_i32_1d *a_res = NULL; // must be freed with futhark_free_i32_1d()
  struct futhark_i32_1d *b_res = NULL;
  struct futhark_i32_1d *c_res = NULL;
    
  struct futhark_i32_1d *a_arr = futhark_new_i32_1d(ctx, in_a, l);
  struct futhark_i32_1d *b_arr = futhark_new_i32_1d(ctx, in_b, l);
  struct futhark_i32_1d *c_arr = futhark_new_i32_1d(ctx, in_c, l);

  futhark_entry_main(ctx, &a_res, &b_res, &c_res, a_arr, b_arr, c_arr);
  
  futhark_values_i32_1d(ctx, a_res, out_a);
  futhark_values_i32_1d(ctx, b_res, out_b);
  futhark_values_i32_1d(ctx, c_res, out_c);
  
  //futhark_free_i32_1d()
  
}
