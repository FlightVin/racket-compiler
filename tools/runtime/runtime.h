#ifndef LLRACKET_RUNTIME_H
#define LLRACKET_RUNTIME_H

#include <stddef.h> // For size_t
#include <stdint.h> // For int64_t

void write_int(int v);
void write_bool(int v);

int read_value(int type);

// ADDED: Simple allocation function (using malloc for now)
// Returns a pointer to memory suitable for holding (1 + num_elements) * 8
// bytes. The first 8 bytes are reserved for the tag.
int64_t *
runtime_allocate(int64_t num_elements,
                 int64_t element_size_bytes); // Using int64_t for num_elements
                                              // to match potential LLVM type

#endif // LLRACKET_RUNTIME_H