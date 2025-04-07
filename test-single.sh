#!/bin/bash

# Exit on error
set -e

# Check if a test file was provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <path-to-test-file.rkt>"
    exit 1
fi

TEST_FILE=$1

echo "Building project..."
set -e  # Exit immediately if a command exits with a non-zero status

echo "Cleaning previous build..."
rm -rf build/

export LIBRARY_PATH=$(gcc --print-file-name=libstdc++.so | sed 's|/libstdc++\.so||'):$LIBRARY_PATH
export LD_LIBRARY_PATH=$(gcc --print-file-name=libstdc++.so | sed 's|/libstdc++\.so||'):$LD_LIBRARY_PATH

echo "Configuring CMake..."
cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_C_COMPILER=/usr/local/bin/clang \
    -DCMAKE_CXX_COMPILER=/usr/local/bin/clang++ \
    -S . -B build \
    -DCMAKE_CXX_FLAGS="-fno-rtti"

echo "Building the project..."
cmake --build ./build

# Define paths
PROJECT_ROOT=$(dirname "$(realpath "$0")")
BUILD_DIR="$PROJECT_ROOT/build"
LLRACKET="$BUILD_DIR/bin/llracket"
RUNTIME="$PROJECT_ROOT/tools/runtime/runtime.c"

# Check if the necessary files exist
if [ ! -f "$LLRACKET" ]; then
    echo "Error: llracket binary not found at $LLRACKET"
    exit 1
fi

if [ ! -f "$RUNTIME" ]; then
    echo "Error: runtime.c not found at $RUNTIME"
    exit 1
fi

echo "Running test: $TEST_FILE"
python3 "$PROJECT_ROOT/tests/test_script.py" --llracket "$LLRACKET" --runtime "$RUNTIME" --build-dir "$BUILD_DIR" --single-test "$TEST_FILE"

echo "Test execution completed."
