#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <stdio.h>
#include <stdint.h>

#include "hashtbl/hashtbl.h"

#define INT64_EQ(a, b) (*(const int64_t *)(a) == (b)->key)
#define INT64_HASH(x)                                                          \
  (hashtbl_default_hash((const int64_t *)(x), sizeof(int64_t)))
HASHMAP_NEW_KIND(i64map, int64_t, int64_t, 8, DEFAULT_ALLOC, DEFAULT_COPY,
                 DEFAULT_DEL, INT64_EQ, DEFAULT_FREE, DEFAULT_GET, INT64_HASH,
                 DEFAULT_INIT, DEFAULT_MOVE)

#define I64map_val(v) (*((i64map_t **) Data_custom_val(v)))

void intmap_finalize(value map) {
  i64map_destroy(I64map_val(map));
}

const static struct custom_operations intmap_ops = {
  "intmap",
  intmap_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value intmap_create_stub(value cap) {
  CAMLparam1 (cap);
  value res = caml_alloc_custom(&intmap_ops, sizeof(i64map_t *), 0, 1);
  I64map_val(res) = i64map_new(Int_val(cap));
  CAMLreturn(res);
}

value intmap_clear_stub(value map) {
  CAMLparam1(map);
  i64map_clear(I64map_val(map));
  CAMLreturn(Val_unit);
}

value intmap_reset_stub(value map) {
  CAMLparam1(map);
  i64map_clear(I64map_val(map));
  CAMLreturn(Val_unit);
}

value intmap_add_stub(value map, value key, value val) {
  CAMLparam3(map, key, val);
  int64_t x = Long_val(key);
  i64map_insert_t ins = i64map_deferred_insert(Data_custom_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&ins.iter);
  entry->key = x;
  entry->val = Long_val(val);
  CAMLreturn(Val_unit);
}

value intmap_add_untagged_stub(value map, intnat key, intnat val) {
  CAMLparam1(map);
  int64_t x = key;
  i64map_insert_t ins = i64map_deferred_insert(Data_custom_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&ins.iter);
  entry->key = x;
  entry->val = val;
  CAMLreturn(Val_unit);
}

value intmap_remove_stub(value map, value key) {
  CAMLparam2(map, key);
  int64_t x = Long_val(key);
  i64map_erase(I64map_val(map), &x);
  CAMLreturn(Val_unit);
}

value intmap_remove_untagged_stub(value map, intnat key) {
  CAMLparam1(map);
  int64_t x = key;
  i64map_erase(I64map_val(map), &x);
  CAMLreturn(Val_unit);
}

value intmap_find_stub(value map, value key) {
  CAMLparam2(map, key);
  int64_t x = Long_val(key);
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  if (entry == NULL) {
    caml_raise_not_found();
  } else {
    CAMLreturn(Val_long(entry->val));
  }
}

intnat intmap_find_untagged_stub(value map, intnat key) {
  CAMLparam1(map);
  int64_t x = key;
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  if (entry == NULL) {
    caml_raise_not_found();
  } else {
    CAMLreturnT(intnat, entry->val);
  }
}

value intmap_find_opt_stub(value map, value key) {
  CAMLparam2(map, key);
  int64_t x = Long_val(key);
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  if (entry == NULL) {
    CAMLreturn(Val_none);
  } else {
    CAMLreturn(caml_alloc_some(Val_long(entry->val)));
  }
}

value intmap_find_opt_untagged_stub(value map, intnat key) {
  CAMLparam1(map);
  int64_t x = key;
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  if (entry == NULL) {
    CAMLreturn(Val_none);
  } else {
    CAMLreturn(caml_alloc_some(Val_long(entry->val)));
  }
}

value intmap_mem_stub(value map, value key) {
  CAMLparam2(map, key);
  int64_t x = Long_val(key);
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  CAMLreturn(Val_bool(entry != NULL));
}

intnat intmap_mem_untagged_stub(value map, intnat key) {
  CAMLparam1(map);
  int64_t x = key;
  i64map_iter_t iter = i64map_find(I64map_val(map), &x);
  i64map_entry_t *entry = i64map_iter_get(&iter);
  CAMLreturnT(intnat, entry != NULL);
}

value intmap_iter_stub(value func, value map) {
  CAMLparam2(map, func);
  i64map_iter_t iter = i64map_iter(I64map_val(map));
  i64map_entry_t *entry = i64map_iter_get(&iter);
  while (entry != NULL) {
    caml_callback2(func, Val_long(entry->key), Val_long(entry->val));
    entry = i64map_iter_next(&iter);
  }
  CAMLreturn(Val_unit);
}

value intmap_fold_stub(value func, value map, value acc) {
  CAMLparam3(func, map, acc);
  i64map_iter_t iter = i64map_iter(I64map_val(map));
  i64map_entry_t *entry = i64map_iter_get(&iter);
  while (entry != NULL) {
    acc = caml_callback3(func, Val_long(entry->key), Val_long(entry->val), acc);
    entry = i64map_iter_next(&iter);
  }
  CAMLreturn(acc);
}

value intmap_length_stub(value map) {
  CAMLparam1(map);
  CAMLreturn(Val_int(i64map_size(I64map_val(map))));
}
