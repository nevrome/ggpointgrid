// Generated by Futhark 0.22.0
// git: 65ba7c2 (Thu Jan 20 18:22:15 2022 +0100)
#pragma once


// Headers\n")
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <float.h>

#ifdef __cplusplus
extern "C" {
#endif

// Initialisation
struct futhark_context_config;
struct futhark_context_config *futhark_context_config_new(void);
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int flag);
struct futhark_context;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg,
                                            const char *param_name,
                                            size_t param_value);
int futhark_get_tuning_param_count(void);
const char *futhark_get_tuning_param_name(int);
const char *futhark_get_tuning_param_class(int);

// Arrays
struct futhark_i32_1d;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx, const
                                          int32_t *data, int64_t dim0);
struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0);
int futhark_free_i32_1d(struct futhark_context *ctx,
                        struct futhark_i32_1d *arr);
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data);
unsigned char *futhark_values_raw_i32_1d(struct futhark_context *ctx,
                                         struct futhark_i32_1d *arr);
const int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                                    struct futhark_i32_1d *arr);

// Opaque values


// Entry points
int futhark_entry_main(struct futhark_context *ctx,
                       struct futhark_i32_1d **out0,
                       struct futhark_i32_1d **out1,
                       struct futhark_i32_1d **out2, const
                       struct futhark_i32_1d *in0, const
                       struct futhark_i32_1d *in1, const
                       struct futhark_i32_1d *in2);

// Miscellaneous
int futhark_context_sync(struct futhark_context *ctx);
char *futhark_context_report(struct futhark_context *ctx);
char *futhark_context_get_error(struct futhark_context *ctx);
void futhark_context_set_logging_file(struct futhark_context *ctx, FILE *f);
void futhark_context_pause_profiling(struct futhark_context *ctx);
void futhark_context_unpause_profiling(struct futhark_context *ctx);
int futhark_context_clear_caches(struct futhark_context *ctx);
#define FUTHARK_BACKEND_c

#ifdef __cplusplus
}
#endif
