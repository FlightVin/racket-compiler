#ifndef LLRACKET_RUNTIME_H
#define LLRACKET_RUNTIME_H

#include <stddef.h> // For size_t
#include <stdint.h> // For int64_t

// Global pointers for heap management (declarations)
// These will be defined in runtime.c
extern int64_t *free_ptr;
extern int64_t *fromspace_begin;
extern int64_t *fromspace_end;
extern int64_t *tospace_begin;
extern int64_t *tospace_end;
// Global pointer for root stack (declaration)
extern int64_t *
    *rootstack_begin; // Pointer to the start of the root stack array

// Function Declarations
void write_int(int v);
void write_bool(int v);
int read_value(int type);

int64_t *runtime_allocate(int64_t num_elements, int64_t element_size_bytes);

// <<< ADDED Declaration for initialize >>>
void initialize(int64_t rootstack_size, int64_t heap_size);

// <<< ADDED Declaration for collect (needed by compiler later) >>>
// Parameters: pointer to top of root stack, bytes requested
void collect(int64_t **rootstack_ptr, int64_t bytes_requested);

#endif // LLRACKET_RUNTIME_H