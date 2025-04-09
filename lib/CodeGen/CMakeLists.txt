add_library(llracketCodeGen
    CodeGen.cpp
    CodeGenProgram.cpp
    CodeGenLiterals.cpp
    CodeGenVariables.cpp
    CodeGenControlFlow.cpp
    CodeGenPrimitives.cpp
)

target_link_libraries(llracketCodeGen PRIVATE LLVMCore LLVMSupport)

# Depending on project structure, you might need:
# target_include_directories(llracketCodeGen PRIVATE
#    ${CMAKE_SOURCE_DIR}/include
#    ${LLVM_INCLUDE_DIRS}
# )
# Or if CodeGen needs access to e.g. Basic:
# target_include_directories(llracketCodeGen PRIVATE
#    ${CMAKE_CURRENT_SOURCE_DIR}/../.. # Adjust path to root include
# )
# However, typically includes from linked libraries (like Basic via main executable link)
# or top-level includes are sufficient. Check if compile errors point to missing headers.
target_include_directories(llracketCodeGen PRIVATE ${LLVM_INCLUDE_DIRS}) # Keep this