# LLRacket: A Racket-like Language Compiler with LLVM

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

LLRacket is a statically-typed, Racket-like programming language compiler that targets LLVM Intermediate Representation (IR). The compiler is implemented in C++ and supports a variety of language features including functions, conditionals, loops, and basic data types.

## Who made this?

Aryan and Vineeth, Vineeth and Aryan (ofc, the compilers TA and profs guided us - couldn't - wouldn't - have done it without them)

## Documentation

For detailed information about the compiler's implementation:

- [Type System](docs/types.md) - Overview of the static type system and type checking
- [Control Flow](docs/control-flow.md) - Architecture and module interactions

## Features

- **Static Type System**: Supports Integer, Boolean, and Void types with type inference
- **Control Flow**: If-else conditionals, while loops, and begin blocks
- **Functions**: First-class functions with recursion and mutual recursion support
- **Variables**: Mutable variables with lexical scoping
- **Primitive Operations**: Arithmetic, comparison, and logical operations
- **I/O Operations**: Basic input/output functions
- **Vectors**: Fixed-size array-like data structures
- **Type Safety**: Comprehensive compile-time type checking

## Project Structure

```
.
├── include/llracket/     # Header files
│   ├── AST/             # Abstract Syntax Tree definitions
│   ├── Basic/           # Basic types and utilities
│   ├── CodeGen/         # LLVM IR code generation
│   ├── Lexer/          # Lexical analysis
│   ├── Parser/         # Syntax analysis
│   └── Sema/           # Semantic analysis
├── lib/                # Implementation files
├── tests/              # Test cases
│   ├── bool/           # Boolean operation tests
│   ├── fun/            # Function tests
│   ├── int/            # Integer operation tests
│   └── var/            # Variable tests
└── tools/              # Compiler tools and runtime
    └── runtime/        # Runtime support library
```

## Getting Started

### Prerequisites

- LLVM 20.x
- Clang 20.x
- CMake 3.20 or later
- Ninja build system
- Python 3.x (for running tests)

### Building from Source

1. Clone the repository:
   ```bash
   git clone https://github.com/FlightVin/racket-compiler.git
   cd racket-compiler
   ```

2. Configure the build:
   ```bash
   cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug \
         -DCMAKE_C_COMPILER=clang \
         -DCMAKE_CXX_COMPILER=clang++ \
         -S . -B build
   ```

3. Build the project:
   ```bash
   cmake --build ./build
   ```

The compiled `llracket` binary will be available at `build/bin/llracket`.

## Usage

### Running the Compiler

To compile a Racket source file:

```bash
./build/bin/llracket input.rkt -o output.ll
```

### Running Tests

Run the test suite using the provided script:

```bash
./test-single.sh tests/fun/recursion_fac.rkt
```

Or run all tests:

```bash
cd build
ninja check
```

## Language Reference

### Data Types

- **Integer**: 32-bit signed integers
- **Boolean**: `#t` (true) or `#f` (false)
- **Void**: Represents the absence of a value
- **Vector**: Fixed-size arrays (e.g., `(vector 1 2 3)`)

### Control Structures

#### If Expression
```racket
(if condition
    then-expr
    else-expr)
```

#### While Loop
```racket
(while condition
  body-expr)
```

#### Begin Block
```racket
(begin
  expr1
  expr2
  ...
  result-expr)
```

### Functions

#### Function Definition
```racket
(define (function-name [param1 : Type1] [param2 : Type2] ...) : ReturnType
  body-expr)
```

#### Function Application
```racket
(function-name arg1 arg2 ...)
```

### Variables

#### Let Binding
```racket
(let ([var1 expr1]
      [var2 expr2])
  body-expr)
```

#### Variable Assignment
```racket
(set! var-name new-value)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Based on the LLRacket compiler project
- Uses LLVM for code generation
- Inspired by Racket and Scheme programming languages
