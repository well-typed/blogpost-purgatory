#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

void rust_wrapper_rcgen_generate_simple_self_signed(const uint8_t *alt_names,
                                                    uintptr_t alt_names_len,
                                                    uint8_t *out,
                                                    uintptr_t *out_len);
