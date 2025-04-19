#include "runtime.h"
#include <stdio.h>
#include <stdlib.h> // For malloc, exit
#include <string.h>
#include <stdint.h> // For int64_t
#include <stddef.h> // For size_t

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
        fprintf(stderr, "Error: read_int failed - expected integer, got '%s'\n", token);
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
    fprintf(stderr, "Error: read_bool failed - expected #t or #f, got '%s'\n", token);
    exit(1);
  } else {
    fprintf(stderr, "Error: read_value called with invalid type hint %d\n", type);
    exit(1);
  }
}

/**
 * @brief Allocates memory on the heap for a tuple/vector.
 *
 * This is a placeholder using malloc. A real implementation would use a
 * garbage-collected heap. The first element (index 0) is reserved for the tag.
 *
 * @param num_elements The number of elements the vector will hold (excluding the tag).
 * @param element_size_bytes The size of each element (typically 8 for 64-bit values).
 * @return A pointer to the allocated memory (int64_t*), or NULL on failure.
 */
int64_t* runtime_allocate(int64_t num_elements, int64_t element_size_bytes) {
    if (num_elements < 0 || element_size_bytes <= 0) {
        fprintf(stderr, "Error: runtime_allocate called with invalid size %lld or element size %lld\n",
                (long long)num_elements, (long long)element_size_bytes);
        exit(1);
    }
    // +1 for the tag
    size_t total_size = (size_t)(num_elements + 1) * (size_t)element_size_bytes;
    int64_t* ptr = (int64_t*)malloc(total_size);
    if (ptr == NULL) {
        fprintf(stderr, "Error: runtime_allocate failed - malloc returned NULL\n");
        exit(1);
    }
    // Optional: Initialize memory to zero or a known pattern for debugging
    // memset(ptr, 0, total_size);
    return ptr;
}