#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

#include "hwsl2_core/sl2-inl.h"

typedef struct {
  sl2_t inner;
} sl2_wrapper;

#define Sl2_val(v) (((sl2_wrapper *)Data_custom_val(v))->inner)

#define make_sl2_buf()                                                         \
  (caml_alloc_small(sizeof(sl2_wrapper) / sizeof(value) + 1, Abstract_tag))

value sl2_unit_stub(value unit) {
  CAMLparam1(unit);
  value res = make_sl2_buf();
  sl2_unit(Sl2_val(res));
  CAMLreturn(res);
}

value sl2_mul_stub(value a, value b) {
  CAMLparam2(a, b);
  value res = make_sl2_buf();
  sl2_mul(Sl2_val(a), Sl2_val(b), Sl2_val(res));
  CAMLreturn(res);
}

value sl2_valid_stub(value a) {
  CAMLparam1(a);
  CAMLreturn(Val_bool(sl2_valid(Sl2_val(a))));
}

value sl2_eq_stub(value a, value b) {
  CAMLparam2(a, b);
  CAMLreturn(Val_bool(sl2_eq(Sl2_val(a), Sl2_val(b))));
}

value sl2_cmp_stub(value a, value b) {
  CAMLparam2(a, b);
  CAMLreturn(Val_int(sl2_eq(Sl2_val(a), Sl2_val(b))));
}

value sl2_from_int_stub(value i) {
  CAMLparam1(i);
  int64_t x = Long_val(i);
  value res = make_sl2_buf();
  sl2_unit(Sl2_val(res));
  sl2_mul_buf_left(Sl2_val(res), (unsigned char *)&x, sizeof(x));
  CAMLreturn(res);
}

// We are just using the top 64 bit as the hash value.
// We are relying on two facts:
//   0: A hit will check the full hash.
//   1: The hash itself already evenly distribute the bits.
// (0) say the worst case is a hash attack, and (1) say it is hard.
value sl2_hash_stub(value sl2) {
  CAMLparam1(sl2);
  CAMLreturn(Val_int(*(int64_t *)Sl2_val(sl2)));
}
