#include "LLRacket.h"
#include "llracket/AST/AST.h" // Make sure AST is included for Tree pointer
#include "llracket/Basic/Diagnostic.h"
#include "llracket/Basic/Type.h" // Include Type for the map key
#include "llracket/CodeGen/CodeGen.h"
#include "llracket/Lexer/Lexer.h"
#include "llracket/Parser/Parser.h"
#include "llracket/Sema/Sema.h"
#include <llvm/ADT/DenseMap.h>   // Include DenseMap for the type map
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SourceMgr.h>

static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::desc("<input file>"),
                                        llvm::cl::Required);
static llvm::cl::opt<std::string> Output("o", llvm::cl::desc("Output file"),
                                         llvm::cl::value_desc("filename"),
                                         llvm::cl::init("a.ll"));

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::cl::ParseCommandLineOptions(argc_, argv_, "LLRacket compiler\n");

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
      llvm::MemoryBuffer::getFile(Input);

  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << Input << ": " << BufferError.message()
                 << "\n";
    return 1;
  }

  // Source manager class to manage source buffers
  llvm::SourceMgr SrcMgr; // Removed 'new' - manage on stack
  DiagnosticsEngine Diags(SrcMgr);

  // Transfer ownership of the buffer to SourceMgr
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  LLRacket Compiler(&SrcMgr, Diags); // Pass SrcMgr by pointer if needed by LLRacket, or adjust LLRacket constructor

  return Compiler.exec();
}

int LLRacket::exec() {
  // 1. Lexing
  Lexer Lex(*SrcMgr, Diags);

  // 2. Parsing
  Parser P(Lex, Diags);
  AST *Tree = P.parse();
  // Check for parsing errors (reported by Parser via Diags)
  if (!Tree || Diags.numErrors()) {
    llvm::errs() << "Syntax error occurred during parsing.\n";
    return 1; // Return distinct code for syntax errors
  }

  // 3. Semantic Analysis (Type Checking)
  Sema S(Diags); // Instantiate Sema with the DiagnosticsEngine
  bool typeCheckSuccessful = S.typeCheck(Tree); // Use the typeCheck method

  // Check for semantic/type errors
  if (!typeCheckSuccessful || Diags.numErrors()) {
      // Diags engine would have already printed specific errors
      llvm::errs() << "Semantic or Type error occurred.\n";
      return 2; // Return distinct code for semantic/type errors
  }
  // Retrieve the type results map after successful check
  const llvm::DenseMap<Expr *, ExprType>& typeResults = S.getExprTypes();

  // 4. Code Generation
  // Instantiate CodeGen, passing the type results map
  // CodeGen CG(Module.get(), Ctx.get()); // <- REMOVE OR COMMENT OUT THIS INCORRECT LINE
  CodeGen CG(Module.get(), Ctx.get(), &typeResults); // <- USE THIS CORRECT LINE (pass address of map)

  // Compile the AST to LLVM IR
  CG.compile(Tree);

  // Optional: Print the generated IR to standard output
  // Module->print(llvm::outs(), nullptr);

  // Save the module to the specified output file
  saveModuleToFile(Output);

  // Assuming successful compilation if we reach here
  return 0;
}