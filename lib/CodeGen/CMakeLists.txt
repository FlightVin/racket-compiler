add_library(
    llracketCodeGen
    CodeGen.cpp
    CodeGenVector.cpp # ADDED
    CodeGenProgram.cpp
    CodeGenLiterals.cpp
    CodeGenVariables.cpp
    CodeGenControlFlow.cpp
    CodeGenPrimitives.cpp
)

target_link_libraries(llracketCodeGen PRIVATE LLVMCore LLVMSupport)

target_include_directories(llracketCodeGen PRIVATE ${LLVM_INCLUDE_DIRS}) # Keep this