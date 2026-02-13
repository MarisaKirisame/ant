#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdint.h>
#include <string.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
typedef struct {
  LARGE_INTEGER value;
  uint8_t is_initialized;
} timestamp_t;
static LARGE_INTEGER frequency;
static int frequency_initialized = 0;

static void init_frequency() {
  if (!frequency_initialized) {
    QueryPerformanceFrequency(&frequency);
    frequency_initialized = 1;
  }
}
#else
#include <time.h>
typedef struct {
  struct timespec value;
  uint8_t is_initialized;
} timestamp_t;
#endif

#define Timestamp_val(v) ((timestamp_t *)Data_custom_val(v))

static struct custom_operations timer_ops = {
  "timer_buffer",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ns_timer_create(value v_unit) {
  CAMLparam1(v_unit);
  value res = caml_alloc_custom(&timer_ops, sizeof(timestamp_t), 0, 1);
  memset(Data_custom_val(res), 0, sizeof(timestamp_t));

#if defined(_WIN32) || defined(_WIN64)
  init_frequency();
#endif

  CAMLreturn(res);
}

CAMLprim value ns_timer_record(value v_timer) {
  CAMLparam1(v_timer);
  timestamp_t *t = Timestamp_val(v_timer);
  
#if defined(_WIN32) || defined(_WIN64)
  QueryPerformanceCounter(&t->value);
#elif defined(APPLE)
  clock_gettime(CLOCK_MONOTONIC_RAW, &t->value);
#else
  clock_gettime(CLOCK_MONOTONIC, &t->value);
#endif

  t->is_initialized = 1;
  
  CAMLreturn(Val_unit);
}

CAMLprim value ns_timer_diff(value v_t1, value v_t2) {
  CAMLparam2(v_t1, v_t2);
  timestamp_t *t1 = Timestamp_val(v_t1);
  timestamp_t *t2 = Timestamp_val(v_t2);
  uint64_t diff_ns;
  
  if (!t1->is_initialized || !t2->is_initialized) {
    caml_failwith("Both timestamps must be initialized with `timer_record` before calculating difference");
  }

#if defined(_WIN32) || defined(_WIN64)
  diff_ns = (uint64_t)(((double)(t2->value.QuadPart - t1->value.QuadPart) * 1000000000.0) / (double)frequency.QuadPart);
#else
  diff_ns = (uint64_t)(t2->value.tv_sec - t1->value.tv_sec) * 1000000000ULL + (uint64_t)(t2->value.tv_nsec - t1->value.tv_nsec);
#endif

  CAMLreturn(caml_copy_int64(diff_ns));
}
