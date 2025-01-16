#include <stdint.h>
#include <caml/mlvalues.h>
#include <assert.h>

#include "hwsl2_core/sl2-inl.h"


/* Align upwards - bit mask mode (hence _b) */
static inline uint8_t *align_upwards_b(uint8_t *stack, uintptr_t align) {
    assert(align > 0 && (align & (align - 1)) == 0); /* Power of 2 */
    assert(stack != 0);

    uintptr_t addr  = (uintptr_t)stack;
    addr = (addr + (align - 1)) & -align;   // Round up to align-byte boundary
    assert(addr >= (uintptr_t)stack);
    return (uint8_t *)addr;
}

psl2_t align(void* x) {
    return (psl2_t)align_upwards_b(x, 16 * 8);
}

void sl2_unit_stub(void* a) {
    sl2_unit(align(a));
}

void sl2_mul_stub(void* c, void* a, void* b) {
    sl2_mul(align(c), align(a), align(b));
}

value sl2_valid_stub(void* a) {
    return Val_bool(sl2_valid(align(a)));
}

value sl2_eq_stub(void* a, void* b) {
    return Val_bool(sl2_eq(align(a), align(b)));
}

value sl2_cmp_stub(void* a, void* b) {
    return Val_int(sl2_cmp(align(a), align(b)));
}

void sl2_from_int_stub(void* b, value a) {
    psl2_t h = align(b);
    int64_t i = Long_val(a);
    sl2_mul_buf_right(h, (unsigned char *)&i, 8);
}

// We are just using the top 64 bit as the hash value. 
// We are relying on two facts:
//   0: A hit will check the full hash.
//   1: The hash itself already evenly distribute the bits.
// (0) say the worst case is a hash attack, and (1) say it is hard.
value sl2_hash_stub(void* a) {
    return Val_int(((int64_t*)align(a))[0]);
}
