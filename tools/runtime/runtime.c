#include "runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // Needed for strncmp (though not used in simplified read)
#include <stdint.h> // Include for int32_t

/**
 * @brief Writes an integer value to standard output (stdout), followed by a newline.
 *
 * @param v The integer value to write (passed as i32 from LLVM).
 * @return void
 */
void write_int(int32_t v) {
  printf("%d\n", v); // %d is appropriate for int32_t
  fflush(stdout); // Ensure output is flushed immediately
}

/**
 * @brief Writes a boolean value to standard output (stdout), followed by a newline.
 *
 * @param v     Passed as i32 from LLVM. If 0, writes "#f". If non-zero, writes "#t".
 * @return      void
 */
void write_bool(int32_t v) {
  if (v == 0){
    printf("#f\n");
  } else {
    printf("#t\n");
  }
  fflush(stdout); // Ensure output is flushed immediately
}

/**
 * @brief Reads an integer value from standard input (stdin).
 *
 * @return      On success: The integer value read (as i32).
 *              On failure: Prints an error message to stderr and exits the program with status 1.
 *
 * @note        Assumes input is a valid integer representation. Skips leading whitespace.
 */
int32_t read_value() {
  int32_t val;
  // Use scanf to read an integer. It automatically skips leading whitespace.
  if (scanf("%d", &val) != 1) {
    // Check for EOF specifically
    if (feof(stdin)) {
        fprintf(stderr, "Runtime Error: Reached end-of-file while trying to read an integer.\n");
    } else {
        fprintf(stderr, "Runtime Error: Failed to read integer input. Invalid format?\n");
    }
    // Clear potential error flags from scanf if needed, although we exit anyway
    // clearerr(stdin);
    exit(1); // Exit on read failure
  }
  return val;
}