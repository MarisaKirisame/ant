#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdlib.h>

#include "hwsl2_core/sl2-inl.h"

#define Sl2_val(v) (*((psl2_t *)Data_custom_val(v)))

const static struct custom_operations sl2_ops = {"sl2",
                                                 custom_finalize_default,
                                                 custom_compare_default,
                                                 custom_hash_default,
                                                 custom_serialize_default,
                                                 custom_deserialize_default,
                                                 custom_compare_ext_default,
                                                 custom_fixed_length_default};

static value make_sl2_buf() {
  value res = caml_alloc_custom(&sl2_ops, sizeof(psl2_t), 0, 1);
  psl2_t psl2 = aligned_alloc(16, sizeof(sl2_t));
  if (psl2 == NULL)
    caml_raise_out_of_memory();
  Sl2_val(res) = psl2;
  return res;
}

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
  sl2_mul_buf_right(Sl2_val(res), (unsigned char *)&x, 8);
  CAMLreturn(res);
}

// We are just using the top 64 bit as the hash value.
// We are relying on two facts:
//   0: A hit will check the full hash.
//   1: The hash itself already evenly distribute the bits.
// (0) say the worst case is a hash attack, and (1) say it is hard.
value sl2_hash_stub(value sl2) {
  CAMLparam1(sl2);
  CAMLreturn(Val_int(Sl2_val(sl2)[0][0][0]));
}
