add_library(llracketSema
    Sema.cpp
    SemaProgram.cpp
    SemaLiteralsVars.cpp
    SemaBindingScope.cpp
    SemaControlFlow.cpp
    SemaPrimitives.cpp
)

target_link_libraries(llracketSema PRIVATE LLVMCore LLVMSupport)

# Adjust include directories if necessary, similar to CodeGen CMakeLists.txt
# Usually inherited includes are sufficient.
target_include_directories(llracketSema PRIVATE ${LLVM_INCLUDE_DIRS})