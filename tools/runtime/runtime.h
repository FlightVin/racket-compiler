#ifndef LLRACKET_RUNTIME_H
#define LLRACKET_RUNTIME_H

#include <stdint.h> // Include for int32_t if used

/**
 * @brief Writes an integer value to standard output (stdout), followed by a newline.
 *
 * @param v The integer value to write (passed as i32 from LLVM).
 * @return void
 */
void write_int(int32_t v);

/**
 * @brief Writes a boolean value to standard output (stdout), followed by a newline.
 *
 * @param v     Passed as i32 from LLVM. If 0, writes "#f". If non-zero, writes "#t".
 * @return      void
 */
void write_bool(int32_t v);

/**
 * @brief Reads an integer value from standard input (stdin).
 *
 * @return      On success: The integer value read (as i32).
 *              On failure: Prints an error message to stderr and exits the program with status 1.
 *
 * @note        Assumes input is a valid integer representation.
 */
int32_t read_value(); // Simplified: always reads an integer


#endif // LLRACKET_RUNTIME_H