#include "LLRacket.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Diagnostic.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/CodeGen/CodeGen.h"
#include "llracket/Lexer/Lexer.h"
#include "llracket/Parser/Parser.h"
#include "llracket/Sema/Sema.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SourceMgr.h>

// Command line options remain the same
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

  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  LLRacket Compiler(&SrcMgr, Diags);
  return Compiler.exec();
}

int LLRacket::exec() {
  // 1. Lexing
  Lexer Lex(*SrcMgr, Diags);

  // 2. Parsing
  Parser P(Lex, Diags);
  AST *Tree = P.parse();
  if (!Tree || Diags.numErrors()) {
    llvm::errs() << "Syntax error occurred during parsing.\n";
    return 1;
  }
  llvm::outs() << "Parsing completed" << "\n";

  // 3. Semantic Analysis (Type Checking)
  Sema S(Diags);
  bool typeCheckSuccessful = S.typeCheck(Tree);

  if (!typeCheckSuccessful || Diags.numErrors()) {
      llvm::errs() << "Semantic or Type error occurred.\n";
      return 2;
  }
  // Retrieve the type results map (now map<Expr*, Type*>)
  const llvm::DenseMap<Expr *, Type*>& typeResults = S.getExprTypes();
  llvm::outs() << "Sem completed" << "\n";

  // 4. Code Generation
  // Instantiate CodeGen, passing the type results map
  // Note: CodeGen constructor now only takes Module*, LLVMContext*, and the map pointer
  CodeGen CG(Module.get(), Ctx.get(), &typeResults);
  llvm::outs() << "Codegen created" << "\n";

  CG.compile(Tree);
  llvm::outs() << "Codegen completed" << "\n";

  saveModuleToFile(Output);

  return 0;
}