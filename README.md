[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/Jc7KCP7q)
# LLVM Racket Compiler

---

## Project Details

For each stage of your project, you are expected to implement language features incrementally. It will ultimately be up to you how to approach this. You are provided with an end-to-end compiler for the Lint language (*EoC Figure 1.1*) as a sample project. You can either use this template as-is or adapt it to suit your needs as the project progresses. Certain files are expected to be updated throughout the course, and these will be highlighted below.

The instructions for each stage will be posted on Moodle. To get started, you are expected to implement the Lvar language as described in *EoC Figure 2.1*.

---

## Project Template

You are provided with a Racket frontend for the Lint language. The project follows the directory structure described in Chapter 3 of the textbook and implements a simple visitor pattern for both semantic analysis and code generation, as outlined in Chapter 2 of the textbook. Additionally, basic runtime and testing functionality are included, which you can extend as needed.

To begin, clone this repository and implement your features on top of the provided template.

---

## Project Instructions

### LLVM Installation

To run this project, you will need to have LLVM 20.x and Clang 20.x installed. You can either download precompiled binaries/packages for your distribution or build LLVM from source (recommended). Installation steps were covered in a previous tutorial. If you encounter issues, feel free to contact the TA for assistance.

### Running the Project with CMake

Before running the project, ensure you have installed CMake, Ninja, and Python for your operating system. Follow these steps to build the project:

1. **Configure the Project**: Use the Ninja build system and specify your compiler paths explicitly.
   ```sh
   cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=/path/to/clang -DCMAKE_CXX_COMPILER=/path/to/clang++ -S . -B build
   ```
   You may need to create a `build` directory in the root of the repository before running this command.

2. **Build the Project**:
   ```sh
   cmake --build ./build
   ```
   This step compiles the project and places the binary in `./build/bin`.

### Testing

Once the project is built and you have a compiled `llracket` binary, you can test your code against the tests in the `tests` directory by running the following command in the `build` directory:
```sh
ninja check
```

You can write your own test scripts in `tests/my_test_script.py` and define custom testing methods. Note that the `tests/test_script.py` file is likely to be updated during the course, so avoid making irreversible changes to it.

### Incremental Updates

Before each assignment (or when instructed by the instructor/TA), you may need to update the code by pulling changes from GitHub. Run the following command from within the project folder:
```sh
git pull
```

The files most likely to change are `./tests/test_script.py` and the `./tools/runtime` directory. You can freely modify the remaining files with minimal risk of merge conflicts.

