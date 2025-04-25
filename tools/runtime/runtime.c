#include "runtime.h"
#include <stddef.h> // For size_t
#include <stdint.h> // For int64_t
#include <stdio.h>
#include <stdlib.h> // For malloc, exit, free
#include <string.h>

// --- Global Pointers for Heap and Root Stack ---
// Note: Using static linkage might be safer in a larger project,
// but extern as declared in the header works for this setup.
int64_t *free_ptr = NULL;
int64_t *fromspace_begin = NULL;
int64_t *fromspace_end = NULL;
int64_t *tospace_begin = NULL;
int64_t *tospace_end = NULL;
int64_t **rootstack_begin = NULL; // Pointer to the allocated root stack memory

// Store sizes for potential reallocation or checks
static int64_t HEAP_SIZE = 0;
static int64_t ROOTSTACK_SIZE = 0;

/**
 * @brief Writes an integer value to standard output (stdout).
 *
 * @param v     The integer value to write.
 * @return      void
 *
 */
void write_int(int v) { printf("%d\n", v); }

/**
 * @brief Writes a boolean value to standard output (stdout).
 *
 * @param v     If 0, writes a "#f". If non-zero, writes a "#t".
 * @return      void
 *
 */
void write_bool(int v) {
  if (v == 0) {
    printf("#f\n");
  } else {
    printf("#t\n");
  }
}

/**
 * @brief Reads a boolean or integer token from standard input (stdin).
 *
 * @param type  If 0, reads an integer. If 1, reads a boolean (`#t` or `#f`).
 * @return      On success:
 *
 *              - For type 0: The integer value read.
 *
 *              - For type 1: 1 for `#t` (true), 0 for `#f` (false).
 *              On failure: exits program.
 *
 * @note Input tokens must be separated by whitespace (e.g., "#t 123 #f").
 *
 */
int read_value(int type) {
  char token[11]; // Buffer for token + null terminator

  if (scanf("%10s", token) != 1) {
    fprintf(stderr, "Error: read failed - unable to read token from input\n");
    exit(1);
  }

  if (type == 0) { // Expecting Integer
    int val;
    char *endptr;
    val = (int)strtol(token, &endptr, 10);
    if (*endptr != '\0' || token == endptr) { // Check if conversion failed
      fprintf(stderr, "Error: read_int failed - expected integer, got '%s'\n",
              token);
      exit(1);
    }
    return val;
  } else if (type == 1) { // Expecting Boolean
    if (strcmp(token, "#t") == 0) {
      return 1;
    }
    if (strcmp(token, "#f") == 0) {
      return 0;
    }
    fprintf(stderr, "Error: read_bool failed - expected #t or #f, got '%s'\n",
            token);
    exit(1);
  } else {
    fprintf(stderr, "Error: read_value called with invalid type hint %d\n",
            type);
    exit(1);
  }
}

/**
 * @brief Allocates memory on the heap for a tuple/vector.
 *
 * This is a placeholder using malloc. A real implementation would use a
 * garbage-collected heap. The first element (index 0) is reserved for the tag.
 *
 * @param num_elements The number of elements the vector will hold (excluding
 * the tag).
 * @param element_size_bytes The size of each element (typically 8 for 64-bit
 * values).
 * @return A pointer to the allocated memory (int64_t*), or NULL on failure.
 */
int64_t *runtime_allocate(int64_t num_elements, int64_t element_size_bytes) {
  if (num_elements < 0 || element_size_bytes <= 0) {
    fprintf(stderr,
            "Error: runtime_allocate called with invalid size %lld or element "
            "size %lld\n",
            (long long)num_elements, (long long)element_size_bytes);
    exit(1);
  }
  // +1 for the tag
  size_t total_size = (size_t)(num_elements + 1) * (size_t)element_size_bytes;
  int64_t *ptr = (int64_t *)malloc(total_size);
  if (ptr == NULL) {
    fprintf(stderr, "Error: runtime_allocate failed - malloc returned NULL\n");
    exit(1);
  }
  // Optional: Initialize memory to zero or a known pattern for debugging
  // memset(ptr, 0, total_size);
  return ptr;
}

/**
 * @brief Initializes the heap (FromSpace, ToSpace) and the root stack.
 *
 * Allocates memory for two heap spaces and the root stack. Sets global
 * pointers. Exits fatally on allocation failure.
 *
 * @param rootstack_byte_size Size of the root stack in bytes. Must be multiple
 * of 8.
 * @param heap_byte_size Total size of the heap in bytes. Must be even and
 * multiple of 8.
 */
void initialize(int64_t rootstack_byte_size, int64_t heap_byte_size) {
  // Basic validation
  if (heap_byte_size <= 0 ||
      heap_byte_size % 16 !=
          0) { // Ensure even and multiple of 8 (for alignment)
    fprintf(stderr,
            "Error: Heap size must be positive and multiple of 16 bytes. Got "
            "%lld\n",
            (long long)heap_byte_size);
    exit(1);
  }
  if (rootstack_byte_size <= 0 || rootstack_byte_size % 8 != 0) {
    fprintf(stderr,
            "Error: Root stack size must be positive and multiple of 8 bytes. "
            "Got %lld\n",
            (long long)rootstack_byte_size);
    exit(1);
  }

  HEAP_SIZE = heap_byte_size;
  ROOTSTACK_SIZE = rootstack_byte_size;
  int64_t half_heap_size = heap_byte_size / 2;

  // Allocate FromSpace
  fromspace_begin = (int64_t *)malloc(half_heap_size);
  if (fromspace_begin == NULL) {
    fprintf(stderr, "Error: Failed to allocate FromSpace (%lld bytes)\n",
            (long long)half_heap_size);
    exit(1);
  }
  fromspace_end = fromspace_begin +
                  (half_heap_size / sizeof(int64_t)); // Pointer arithmetic
  free_ptr = fromspace_begin; // Initially, free pointer starts at the beginning

  // Allocate ToSpace
  tospace_begin = (int64_t *)malloc(half_heap_size);
  if (tospace_begin == NULL) {
    fprintf(stderr, "Error: Failed to allocate ToSpace (%lld bytes)\n",
            (long long)half_heap_size);
    free(fromspace_begin); // Clean up already allocated memory
    exit(1);
  }
  tospace_end = tospace_begin + (half_heap_size / sizeof(int64_t));

  // Allocate Root Stack (array of int64_t* pointers)
  rootstack_begin = (int64_t **)malloc(rootstack_byte_size);
  if (rootstack_begin == NULL) {
    fprintf(stderr, "Error: Failed to allocate Root Stack (%lld bytes)\n",
            (long long)rootstack_byte_size);
    free(fromspace_begin);
    free(tospace_begin);
    exit(1);
  }

  // Optional: Initialize root stack to NULL pointers for safety
  memset(rootstack_begin, 0, rootstack_byte_size);

  // Debug print (optional)
  // printf("Runtime initialized: Heap size %lld, Root stack size %lld\n", (long
  // long)heap_byte_size, (long long)rootstack_byte_size); printf("  FromSpace:
  // %p - %p\n", (void*)fromspace_begin, (void*)fromspace_end); printf("
  // ToSpace:   %p - %p\n", (void*)tospace_begin, (void*)tospace_end); printf("
  // RootStack: %p - %p\n", (void*)rootstack_begin, (void*)(rootstack_begin +
  // (rootstack_byte_size / sizeof(int64_t*)))); printf("  Free Ptr:  %p\n",
  // (void*)free_ptr);
}
// --- END initialize implementation ---

// --- ADDED: Placeholder Implementation for collect ---
/**
 * @brief Placeholder garbage collection function.
 *
 * In a real implementation, this would perform Cheney's algorithm or similar.
 * For now, it just prints an error and exits, as calling it implies the
 * heap is full with the current simple allocation strategy.
 *
 * @param rootstack_ptr Pointer to the current top of the root stack.
 * @param bytes_requested Number of bytes needed for the allocation that failed.
 */
void collect(int64_t **rootstack_ptr, int64_t bytes_requested) {
  // TODO: Implement actual garbage collection (e.g., Cheney's algorithm)
  fprintf(stderr,
          "\nCOLLECT CALLED: Heap is full or needs collection (requested %lld "
          "bytes).\n",
          (long long)bytes_requested);
  fprintf(stderr, "ERROR: Garbage collection not fully implemented yet.\n");
  // In a real collector, you would copy live objects from FromSpace to ToSpace,
  // swap the spaces, and update pointers.
  exit(1); // Exit because we can't allocate more memory without GC.
}
// --- END collect placeholder ---