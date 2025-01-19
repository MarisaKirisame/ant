#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>

#include "monoid_hash/crc32c.h"

static value m_crc32c_encode_i64(m_crc32c_t x) {
  return caml_copy_int64((int64_t)x.p << 32 | x.m);
}

static m_crc32c_t m_crc32c_decode_i64(value v) {
  int64_t x = Int64_val(v);
  m_crc32c_t res;
  res.p = x >> 32;
  res.m = x & 0xFFFFFFFF;
  return res;
}

value m_crc32c_unit_stub(value unit) {
  m_crc32c_t id = m_crc32c_identity();
  return m_crc32c_encode_i64(id);
}

value m_crc32c_mul_stub(value a, value b) {
  m_crc32c_t a_ = m_crc32c_decode_i64(a);
  m_crc32c_t b_ = m_crc32c_decode_i64(b);
  m_crc32c_t res = m_crc32c_combine(a_, b_);
  return m_crc32c_encode_i64(res);
}

value m_crc32c_from_int_stub(value a) {
  int i = Int_val(a);
  m_crc32c_t res = m_crc32c_fold_bytes((const void *)&i, 4);
  return m_crc32c_encode_i64(res);
}

value m_crc32c_from_char_stub(value a) {
  char c = Int_val(a);
  m_crc32c_t res = m_crc32c_fold_bytes(&c, 1);
  return m_crc32c_encode_i64(res);
}

value m_crc32c_hash_stub(value a) {
  m_crc32c_t a_ = m_crc32c_decode_i64(a);
  uint32_t res = m_crc32c_finalize(a_);
  return Val_int(res);
}
